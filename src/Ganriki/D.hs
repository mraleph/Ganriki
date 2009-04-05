module Ganriki.D (translate) where

import qualified Java.ClassParser as J

import Data.List
import Control.Arrow (second)

import Data.Graph.Inductive (Node, LNode, Gr, mkGraph)
import Data.Graph.Inductive.Graphviz (graphviz')

type PC  = Int
newtype LIR = Lir (Gr () ()) -- [J.VMOp]

instance Show LIR where
  show (Lir g) = graphviz' g

translate :: J.MethodCode -> LIR
translate code = Lir $ mkGraph basicBlocks branches
    where 
        (branches, targets') = bbr 0 0 seq [] [] 
        targets = sort $ nub $ 0:last:targets'
        last    = length $ J.bcCode $ J.mcCode code

        basicBlocks = bbs targets seq
                                -- take n l
        bbs (s:e:lx) l  = (s, ()) : (bbs (e:lx) (drop n l)) where n = e - s
        bbs _        [] = []

        seq  = J.bcCode $ J.mcCode code

        -- branches = concatMap (\pc -> map (\pc' -> (pc, pc', ())) $ branchTargets code ) 

        bbr :: Int -> Int -> [J.VMOp] -> [(Int, Int, ())] -> [Int] -> ([(Int, Int, ())], [Int])
        bbr bss pc (op:ops) bs ts = 
            case branchTargets code op pc of
                []                    -> bbr (pc+1) (pc+1) ops bs ((pc+1):ts)
                [pc'] | (pc+1) == pc' -> bbr bss (pc+1) ops bs ts
                [pc'] | otherwise     -> bbr (pc+1) (pc+1) ops ((goto pc'):bs)     ((pc+1):pc':ts)
                px                    -> bbr (pc+1) (pc+1) ops (map goto px ++ bs) ((pc+1):px ++ ts)
            where goto pc' = (bss, pc', ())
        bbr _ _ [] bs ts = (bs, ts)




branchTargets :: J.MethodCode -> J.VMOp -> PC -> [PC]
branchTargets code op pc = case opType op of 
                            NoThrow          -> [pc+1]
                            Throws ex        -> (pc+1):handlersFor code ex
                            CanBranch pcs    -> (pc+1):pcs
                            AlwaysBranch pcs -> pcs
                            End              -> []
                            
    where handlersFor c ex = []


data OpType = NoThrow
            | Throws [J.Class]
            | CanBranch [PC]
            | AlwaysBranch [PC]
            | End           

opType :: J.VMOp -> OpType
opType op = case op of
    J.ALoad  _ -> throws [J.java_lang_ArrayIndexOutOfBounds, J.java_lang_NullPointerException]
    J.AStore _ -> throws [J.java_lang_ArrayIndexOutOfBounds, J.java_lang_NullPointerException]
    J.Div    t | isIntegral t -> throws [J.java_lang_ArithmeticException]
    J.Rem    t | isIntegral t -> throws [J.java_lang_ArithmeticException] 
    
    J.GotoIf  _ target -> CanBranch    [target]
    J.Goto    target -> AlwaysBranch [target]
    
    J.JSR     _ -> error "JSR/Ret are not supported currently"
    J.Ret     _ -> error "JSR/Ret are not supported currently"
    
    J.Return  _ -> End
    
    J.GetStatic _ -> throws $ fieldResolutionExceptions ++ [J.java_lang_IncompatibleClassChangeException] ++ classInitializationExceptions
    J.PutStatic _ -> throws $ fieldResolutionExceptions ++ [J.java_lang_IncompatibleClassChangeException] ++ classInitializationExceptions
    J.GetField  _ -> throws $ fieldResolutionExceptions ++ [J.java_lang_NullPointerException, J.java_lang_IncompatibleClassChangeException]
    J.PutField  _ -> throws $ fieldResolutionExceptions ++ [J.java_lang_NullPointerException, J.java_lang_IncompatibleClassChangeException]
    
    -- TODO [!] get from throws declaration which exception target method can throw
    J.InvokeSpecial   _ -> throws $ [J.java_lang_Throwable] -- methodResolutionExceptions ++ [NullPointerException, UnsatisfiedLinkError]-- ++ [NoSuchMethodError, IncompatibleClassChangeError, AbstractMethodError]
    J.InvokeStatic    _ -> throws $ [J.java_lang_Throwable] -- methodResolutionExceptions ++ classInitializationExceptions ++ [UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError
    J.InvokeVirtual   _ -> throws $ [J.java_lang_Throwable] -- methodResolutionExceptions ++ [UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError, AbstractMethodError
    J.InvokeInterface _ -> throws $ [J.java_lang_Throwable] -- interfaceMethodResolutionExceptions ++ [NullPointerException, UnsatisfiedLinkError] -- ++ IncompatibleClassChangeError, AbstractMethodError, IllegalAccessError
    
    J.New             _ -> throws $ classResolutionExceptions ++ [J.java_lang_InstantiationError] ++ classInitializationExceptions
    J.ANew            _ -> throws $ [J.java_lang_NegativeArraySizeException]
    J.MultiNew        _ _ -> throws $ classResolutionExceptions ++ [J.java_lang_NegativeArraySizeException] -- IllegalAccessError
    
    J.ALength         -> throws $ [J.java_lang_NullPointerException]
    
    -- TODO [!] we can calculate it more accurately
    J.Throw           -> throws $ [J.java_lang_Throwable]
    
    J.CheckCast       _ -> throws $ classResolutionExceptions ++ [J.java_lang_ClassCastException]
    J.InstanceOf      _ -> throws $ classResolutionExceptions
    
    J.MonitorEnter    -> throws $ [J.java_lang_NullPointerException]
    J.MonitorExit     -> throws $ [J.java_lang_NullPointerException, J.java_lang_IllegalMonitorStateException]
    
    J.LookupSwitch    _ _ -> AlwaysBranch []
    J.TableSwitch     _ _ _ _ -> AlwaysBranch []

    _                 -> NoThrow
  where 
    -- JVMS 5.4.3.4
    interfaceMethodResolutionExceptions = classResolutionExceptions ++ [J.java_lang_IncompatibleClassChangeException, J.java_lang_NoSuchMethodError, J.java_lang_LinkageError]
    -- JVMS 5.4.3.3
    methodResolutionExceptions          = classResolutionExceptions ++ [J.java_lang_IncompatibleClassChangeException, J.java_lang_NoSuchMethodError, J.java_lang_AbstractMethodError, J.java_lang_LinkageError] -- IllegalAccessError,
    -- JVMS 5.4.3.2
    fieldResolutionExceptions           = classResolutionExceptions ++ [J.java_lang_NoSuchFieldError, J.java_lang_LinkageError] -- IllegalAccessError,
    -- JVMS 5.4.3.1
    classResolutionExceptions           = classloadingExceptions ++ [J.java_lang_IllegalAccessError]
    -- JVMS 5.3
    classloadingExceptions              = [J.java_lang_Throwable] -- [ClassNotFoundException, RuntimeException]

    classInitializationExceptions       = [J.java_lang_Throwable]

    throws :: [J.Class] -> OpType
    throws l =  Throws $ filter' l l
        where filter' :: [J.Class] -> [J.Class] -> [J.Class]
              filter' (c:cs) fl = filter' cs' (c:cs') where cs' = filter (`J.isSubClassOf` c) fl
              filter' []     fl = fl

    isIntegral J.TInt  = True
    isIntegral J.TLong = True
    isIntegral _       = False

