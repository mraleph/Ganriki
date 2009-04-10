{-# LANGUAGE BangPatterns #-}
module Ganriki.D (translate) where

import Java.ClassParser

import Data.List
import Control.Arrow (second)

import Data.Graph.Inductive (Node, LNode, Gr, mkGraph)
import Data.Graph.Inductive.Graphviz (graphviz')

newtype Label = Label VMOp

instance Show Label where
  show (Label c) = op2string c

type PC  = Int
newtype LIR = Lir (Gr Label ())

instance Show LIR where
  show (Lir g) = graphviz' g

translate :: LinkageContext -> MethodCode -> LIR
translate ctx code = Lir $ mkGraph basicBlocks branches
    where    
        basicBlocks = zip [0..(n-1)] $ map Label bytecode
        branches    = filter (\(_, pc', _) -> pc' < n) $ bbs 0      bytecode 

        bbs !pc (op:ops) = (map (\pc' -> (pc, pc', ())) $ branchTargets ctx code op pc) ++ bbs (pc+1) ops
        bbs _  []        = []

        n = length bytecode
        bytecode = bcCode $ mcCode code

branchTargets :: LinkageContext -> MethodCode -> VMOp -> PC -> [PC]
branchTargets ctx code op pc =
    case opType op of
        NoThrow            -> [pc+1]
        CanThrow       ex  -> nub $ (pc+1):handlersFor ex
        CanBranch      pcs -> nub $ (pc+1):pcs
        AlwaysThrows   ex  -> nub $ handlersFor ex
        AlwaysBranches pcs -> nub $ pcs
        End                -> []
    where 
        handlersFor :: [Class] -> [PC]
        handlersFor ex = map ehiHandlerPC $ filter (\h -> (ehiStartPC h <= pc) && (pc < ehiEndPC h) && (canCatch h) `any` ex) $ mcHandlers code
        
        canCatch :: ExceptionHandlerInfo -> Class -> Bool
        canCatch h ex = case ehiCatch h of
                            Nothing        -> True
                            Just classname -> resolve ctx classname `isSubClassOf` ex -- TODO: in fact isAssignableFrom (interfaces!)
          


data OpType = NoThrow
            | CanThrow       [Class]
            | AlwaysThrows   [Class]
            | CanBranch      [PC]
            | AlwaysBranches [PC]
            | End

opType :: VMOp -> OpType
opType op = case op of
    ALoad  _ -> canThrow [java_lang_ArrayIndexOutOfBoundsException, java_lang_NullPointerException]
    AStore _ -> canThrow [java_lang_ArrayIndexOutOfBoundsException, java_lang_NullPointerException]
    Div    t | isIntegral t -> canThrow [java_lang_ArithmeticException]
    Rem    t | isIntegral t -> canThrow [java_lang_ArithmeticException]

    GotoIf  _ target -> CanBranch      [target]
    Goto    target   -> AlwaysBranches [target]

    JSR     _ -> error "JSR/Ret are not supported currently"
    Ret     _ -> error "JSR/Ret are not supported currently"

    Return  _ -> End

    GetStatic _ -> canThrow $ fieldResolutionExceptions ++ [java_lang_IncompatibleClassChangeError] ++ classInitializationExceptions
    PutStatic _ -> canThrow $ fieldResolutionExceptions ++ [java_lang_IncompatibleClassChangeError] ++ classInitializationExceptions
    GetField  _ -> canThrow $ fieldResolutionExceptions ++ [java_lang_NullPointerException, java_lang_IncompatibleClassChangeError]
    PutField  _ -> canThrow $ fieldResolutionExceptions ++ [java_lang_NullPointerException, java_lang_IncompatibleClassChangeError]

    -- TODO [!] get from canThrow declaration which exception target method can throw
    InvokeSpecial   _ -> canThrow $ [java_lang_Throwable] -- methodResolutionExceptions ++ [NullPointerException, UnsatisfiedLinkError]-- ++ [NoSuchMethodError, IncompatibleClassChangeError, AbstractMethodError]
    InvokeStatic    _ -> canThrow $ [java_lang_Throwable] -- methodResolutionExceptions ++ classInitializationExceptions ++ [UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError
    InvokeVirtual   _ -> canThrow $ [java_lang_Throwable] -- methodResolutionExceptions ++ [UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError, AbstractMethodError
    InvokeInterface _ -> canThrow $ [java_lang_Throwable] -- interfaceMethodResolutionExceptions ++ [NullPointerException, UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError, AbstractMethodError, IllegalAccessError

    New             _ -> canThrow $ classResolutionExceptions ++ [java_lang_InstantiationError] ++ classInitializationExceptions
    ANew            _ -> canThrow $ [java_lang_NegativeArraySizeException]
    MultiNew        _ _ -> canThrow $ classResolutionExceptions ++ [java_lang_NegativeArraySizeException] -- IllegalAccessError

    ALength         -> canThrow $ [java_lang_NullPointerException]

    -- TODO [!] we can calculate it more accurately
    Throw           -> alwaysThrows $ [java_lang_Throwable]

    CheckCast       _ -> canThrow $ classResolutionExceptions ++ [java_lang_ClassCastException]
    InstanceOf      _ -> canThrow $ classResolutionExceptions

    MonitorEnter    -> canThrow $ [java_lang_NullPointerException]
    MonitorExit     -> canThrow $ [java_lang_NullPointerException, java_lang_IllegalMonitorStateException]

    LookupSwitch    _ _     -> AlwaysBranches []
    TableSwitch     _ _ _ _ -> AlwaysBranches []

    _                 -> NoThrow
  where
    -- JVMS 5.4.3.4
    interfaceMethodResolutionExceptions = classResolutionExceptions ++ [java_lang_IncompatibleClassChangeError, java_lang_NoSuchMethodError, java_lang_LinkageError]
    -- JVMS 5.4.3.3
    methodResolutionExceptions          = classResolutionExceptions ++ [java_lang_IncompatibleClassChangeError, java_lang_NoSuchMethodError, java_lang_AbstractMethodError, java_lang_LinkageError] -- IllegalAccessError,
    -- JVMS 5.4.3.2
    fieldResolutionExceptions           = classResolutionExceptions ++ [java_lang_NoSuchFieldError, java_lang_LinkageError] -- IllegalAccessError,
    -- JVMS 5.4.3.1
    classResolutionExceptions           = classloadingExceptions ++ [java_lang_IllegalAccessError]
    -- JVMS 5.3
    classloadingExceptions              = [java_lang_Throwable] -- [ClassNotFoundException, RuntimeException]

    classInitializationExceptions       = [java_lang_Throwable]

    canThrow     = CanThrow . throws'
    alwaysThrows = AlwaysThrows . throws'

    throws' :: [Class] -> [Class]
    throws' l = filter' l (tail l)
        where filter' :: [Class] -> [Class] -> [Class]
              filter' (c:cs) fl = filter' (clean cs) (c:clean fl) where clean = filter (not . (`isSubClassOf` c))
              filter' []     fl = fl

    isIntegral TInt  = True
    isIntegral TLong = True
    isIntegral _       = False

op2string :: VMOp -> String
op2string op =
    case op of
        Push _ -> "push"   
        Load _ _ -> "load"   
        Store _ _-> "store"  
        ALoad _ -> "aload"  
        AStore _ -> "astore" 
        Pop -> "pop"
        Pop2 -> "pop2"
        Dup -> "dup"
        DupX1 -> "dupx1"
        DupX2 -> "dupx2"
        Dup2 -> "dup2"
        Dup2X1 -> "dup2x1"
        Dup2X2 -> "dup2x2"
        Swap -> "swap"
        Add _ -> "add"  
        Sub _ -> "sub"  
        Mul _ -> "mul"  
        Div _ -> "div"  
        Rem _ -> "rem"  
        Neg _ -> "neg"  
        Shl _ -> "shl"  
        Shr _ -> "shr"  
        UShr _ -> "ushr" 
        And _ -> "and"  
        Or  _ -> "or"   
        XOr _ -> "xor"  
        IInc _ _ -> "iinc" 
        Coerce _ _ -> "coerce" 
        Cmp _ -> "cmp"    
        GotoIf _ _ -> "gotoif" 
        Goto _ -> "goto"   
        JSR _ -> "jsr"    
        Ret _ -> "ret"    
        Return _ -> "return" 
        GetStatic _ -> "getstatic" 
        PutStatic _ -> "putstatic" 
        GetField _ -> "getfield"  
        PutField _ -> "putfield"  
        InvokeSpecial _ -> "invokespecial"   
        InvokeStatic _ -> "invokestatic"    
        InvokeVirtual _ -> "invokevirtual"   
        InvokeInterface _ -> "invokeinterface" 
        New _ -> "new"  
        ANew _ -> "anew" 
        ALength -> "alength"
        Throw -> "throw"
        CheckCast _ -> "checkcast"   
        InstanceOf _ -> "instanceof"  
        MonitorEnter -> "monitorenter"
        MonitorExit -> "monitorexit"
        MultiNew _ _ -> "multinew" 
        LookupSwitch _ _ -> "lookupswitch" 
        TableSwitch _ _ _ _ -> "tableswitch" 

data TranslationContext = Context { stackTop :: Int }

type Translation a = State TranslationContext  

vmop2hop :: VMOp -> Translation HOP
vmop2hop op =
    case op of
        Push _    ->    
        Load _ _  -> "load"   
        Store _ _ -> "store"  
        ALoad _   -> "aload"  
        AStore _  -> "astore" 
        Pop       -> "pop"
        Pop2      -> "pop2"
        Dup       -> "dup"
        DupX1     -> "dupx1"
        DupX2     -> "dupx2"
        Dup2      -> "dup2"
        Dup2X1    -> "dup2x1"
        Dup2X2    -> "dup2x2"
        Swap      -> "swap"
        Add _     ->    do i <- pop
                        j <- pop                       
                        push $ OAdd i j 
        Sub _     -> "sub"  
        Mul _     -> "mul"  
        Div _     -> "div"  
        Rem _     -> "rem"  
        Neg _     -> "neg"  
        Shl _     -> "shl"  
        Shr _     -> "shr"  
        UShr _    -> "ushr" 
        And _     -> "and"  
        Or  _ -> "or"   
        XOr _ -> "xor"  
        IInc _ _ -> "iinc" 
        Coerce _ _ -> "coerce" 
        Cmp _ -> "cmp"    
        GotoIf _ _ -> "gotoif" 
        Goto _ -> "goto"   
        JSR _ -> "jsr"    
        Ret _ -> "ret"    
        Return _ -> "return" 
        GetStatic _ -> "getstatic" 
        PutStatic _ -> "putstatic" 
        GetField _ -> "getfield"  
        PutField _ -> "putfield"  
        InvokeSpecial _ -> "invokespecial"   
        InvokeStatic _ -> "invokestatic"    
        InvokeVirtual _ -> "invokevirtual"   
        InvokeInterface _ -> "invokeinterface" 
        New _ -> "new"  
        ANew _ -> "anew" 
        ALength -> "alength"
        Throw -> "throw"
        CheckCast _ -> "checkcast"   
        InstanceOf _ -> "instanceof"  
        MonitorEnter -> "monitorenter"
        MonitorExit -> "monitorexit"
        MultiNew _ _ -> "multinew" 
        LookupSwitch _ _ -> "lookupswitch" 
        TableSwitch _ _ _ _ -> "tableswitch" 


data HOp = Assign Local Local
         | Phi Local [Locals]
         | Invoke (Maybe Local) Local Method [Locals]
         | InvokeStatic (Maybe Local) Method [Locals]
         | PutField Field Local
         | GetField Local Field
         | PutStaticField Field Local
         | GetStaticField Local Field
         | Return (Maybe Local)
         | Throw Local

newtype HIR = HIR (Gr HOp ())

lir2hir :: LIR -> HIR
lir2hir (Lir l) =         
