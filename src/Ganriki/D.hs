module Ganriki.D (translate) where

import Java.ClassParser

import Debug.Trace

import Data.List
import Control.Arrow (second)

import Data.Graph.Inductive (Node, LNode, Gr, mkGraph)
import Data.Graph.Inductive.Graphviz (graphviz')

newtype Label = Label [VMOp]

instance Show Label where
  show (Label c) = concat $ intersperse "\\n" $ zipWith f [1..n] c where f i l = (show i) ++ ": " ++ (op2string l)
                                                                         n     = length c

type PC  = Int
newtype LIR = Lir (Gr Label ())

instance Show LIR where
  show (Lir g) = graphviz' g

-- Maybe Reader monad?

translate :: LinkageContext -> MethodCode -> LIR
translate ctx code = Lir $ mkGraph basicBlocks branches
    where    
        (branches, targets') = bbr 0 0 seq [] []
        targets = sort $ nub $ 0:last:targets'
        last    = length $ bcCode $ mcCode code

        basicBlocks = bbs targets seq

        bbs (s:e:lx) l  = (s, Label $ take n l) : (bbs (e:lx) (drop n l)) where n = e - s
        bbs _        [] = []

        seq  = bcCode $ mcCode code

        bbr :: Int -> Int -> [VMOp] -> [(Int, Int, ())] -> [Int] -> ([(Int, Int, ())], [Int])
        bbr pc (op:ops) bs ts =
            case branchTargets ctx code op pc of
                []                    -> bbr (pc+1) (pc+1) ops bs ((pc+1):ts)
                [pc'] | (pc+1) == pc' -> bbr bss (pc+1) ops bs ts
                [pc'] | otherwise     -> bbr (pc+1) (pc+1) ops ((goto pc'):bs)     ((pc+1):pc':ts)
                px                    -> bbr (pc+1) (pc+1) ops (map goto px ++ bs) ((pc+1):px ++ ts)
            where goto pc' = (bss, pc', ())
        bbr _ _ [] bs ts = (bs, ts)
    
tt x = trace (show x) x 

branchTargets :: LinkageContext -> MethodCode -> VMOp -> PC -> [PC]
branchTargets ctx code op pc =
    case opType op of
        NoThrow          -> [pc+1]
        Throws ex        | trace (" >>> " ++ (show $ map clsName ex)) True -> tt $ nub $ (pc+1):[] --handlersFor ex
        CanBranch pcs    -> nub $ (pc+1):pcs
        AlwaysBranch pcs -> pcs
        End              -> []
    where 
        handlersFor :: [Class] -> [PC]
        handlersFor ex = tt $ map ehiHandlerPC $ filter (\h -> (ehiStartPC h <= pc) && (pc < ehiEndPC h) && (canCatch h) `any` ex) $ mcHandlers code
        
        canCatch :: ExceptionHandlerInfo -> Class -> Bool
        canCatch h ex = case ehiCatch h of
                            Nothing        -> trace "Nothing" True
                            Just classname -> trace ("resolve " ++ classname) (resolve ctx classname `isSubClassOf` ex) -- TODO: in fact isAssignableFrom (interfaces!)
          


data OpType = NoThrow
            | Throws [Class]
            | CanBranch [PC]
            | AlwaysBranch [PC]
            | End

opType :: VMOp -> OpType
opType op = case tt op of
    ALoad  _ -> throws [java_lang_ArrayIndexOutOfBoundsException, java_lang_NullPointerException]
    AStore _ -> throws [java_lang_ArrayIndexOutOfBoundsException, java_lang_NullPointerException]
    Div    t | isIntegral t -> throws [java_lang_ArithmeticException]
    Rem    t | isIntegral t -> throws [java_lang_ArithmeticException]

    GotoIf  _ target -> CanBranch    [target]
    Goto    target -> AlwaysBranch [target]

    JSR     _ -> error "JSR/Ret are not supported currently"
    Ret     _ -> error "JSR/Ret are not supported currently"

    Return  _ -> End

    GetStatic _ -> throws $ fieldResolutionExceptions ++ [java_lang_IncompatibleClassChangeError] ++ classInitializationExceptions
    PutStatic _ -> throws $ fieldResolutionExceptions ++ [java_lang_IncompatibleClassChangeError] ++ classInitializationExceptions
    GetField  _ -> throws $ fieldResolutionExceptions ++ [java_lang_NullPointerException, java_lang_IncompatibleClassChangeError]
    PutField  _ -> throws $ fieldResolutionExceptions ++ [java_lang_NullPointerException, java_lang_IncompatibleClassChangeError]

    -- TODO [!] get from throws declaration which exception target method can throw
    InvokeSpecial   _ -> throws $ [java_lang_Throwable] -- methodResolutionExceptions ++ [NullPointerException, UnsatisfiedLinkError]-- ++ [NoSuchMethodError, IncompatibleClassChangeError, AbstractMethodError]
    InvokeStatic    _ -> throws $ [java_lang_Throwable] -- methodResolutionExceptions ++ classInitializationExceptions ++ [UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError
    InvokeVirtual   _ -> throws $ [java_lang_Throwable] -- methodResolutionExceptions ++ [UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError, AbstractMethodError
    InvokeInterface _ -> throws $ [java_lang_Throwable] -- interfaceMethodResolutionExceptions ++ [NullPointerException, UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError, AbstractMethodError, IllegalAccessError

    New             _ -> throws $ classResolutionExceptions ++ [java_lang_InstantiationError] ++ classInitializationExceptions
    ANew            _ -> throws $ [java_lang_NegativeArraySizeException]
    MultiNew        _ _ -> throws $ classResolutionExceptions ++ [java_lang_NegativeArraySizeException] -- IllegalAccessError

    ALength         -> throws $ [java_lang_NullPointerException]

    -- TODO [!] we can calculate it more accurately
    Throw           -> throws $ [java_lang_Throwable]

    CheckCast       _ -> throws $ classResolutionExceptions ++ [java_lang_ClassCastException]
    InstanceOf      _ -> throws $ classResolutionExceptions

    MonitorEnter    -> throws $ [java_lang_NullPointerException]
    MonitorExit     -> throws $ [java_lang_NullPointerException, java_lang_IllegalMonitorStateException]

    LookupSwitch    _ _ -> AlwaysBranch []
    TableSwitch     _ _ _ _ -> AlwaysBranch []

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

    throws :: [Class] -> OpType
    throws l =  Throws $ filter' l (tail l)
        where filter' :: [Class] -> [Class] -> [Class]
              filter' (c:cs) fl = filter' (clean cs) (c:clean fl) where clean = filter (not . (`isSubClassOf` c))
              filter' []     fl = trace "filter' done" fl

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
