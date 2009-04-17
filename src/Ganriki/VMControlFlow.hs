module Ganriki.VMControlFlow (targets, vmopTargets, OpType(..)) where

import Java.ClassParser

type PC = Int

data OpType = NoThrow
            | CanThrow       [Class]
            | AlwaysThrows   [Class]
            | CanBranch      [PC]
            | AlwaysBranches [PC]
            | End

targets :: [ExceptionHandlerInfo] -> PC -> OpType -> [(PC, Bool)]
targets ehi pc optype =
    case optype of
        NoThrow            -> [(pc+1, False)]
        CanThrow ex        -> (pc+1, False):(m True $ handlers ex)
        CanBranch      pcs -> (pc+1, False):(m False pcs)
        AlwaysThrows ex    -> m True $ handlers ex
        AlwaysBranches pcs -> m False pcs
        End                -> []
    where 
        handlers ex = map ehiHandlerPC $ filter (\h -> (ehiStartPC h <= pc) && (pc < ehiEndPC h) && (canCatch h) `any` ex) ehi
        m b = map (\x -> (x, b))

        canCatch :: ExceptionHandlerInfo -> Class -> Bool
        canCatch h ex = case ehiCatch h of
                            Nothing    -> True
                            Just clazz -> clazz `isSubClassOf` ex -- TODO: in fact isAssignableFrom (interfaces!)

vmopTargets code pc op = targets code pc (vmopControlFlowType op)

--------------------------------------------------------------------------------------
-- NOTE: we assume that: 
--   1) all classes are resolved and initialized eagerly before method execution 
--          => there will be no linking errors
--   2) there is infinite amount of memory :)
--          => no out of memory exceptions
--------------------------------------------------------------------------------------

vmopControlFlowType :: VMOp -> OpType
vmopControlFlowType op = case op of
    ALoad  _ -> CanThrow [java_lang_ArrayIndexOutOfBoundsException, java_lang_NullPointerException]
    AStore _ -> CanThrow [java_lang_ArrayIndexOutOfBoundsException, java_lang_NullPointerException]
    Div    t | isIntegral t -> CanThrow [java_lang_ArithmeticException]
    Rem    t | isIntegral t -> CanThrow [java_lang_ArithmeticException]

    GotoIf  _ target -> CanBranch      [target]
    Goto    target   -> AlwaysBranches [target]

    JSR     _ -> error "JSR/Ret are not supported currently"
    Ret     _ -> error "JSR/Ret are not supported currently"

    Return  _ -> End
    
    GetStatic _ -> NoThrow
    PutStatic _ -> NoThrow
    GetField  _ -> CanThrow [java_lang_NullPointerException]
    PutField  _ -> CanThrow [java_lang_NullPointerException]

    InvokeSpecial   _ -> CanThrow [java_lang_Throwable]
    InvokeStatic    _ -> CanThrow [java_lang_Throwable]
    InvokeVirtual   _ -> CanThrow [java_lang_Throwable]
    InvokeInterface _ -> CanThrow [java_lang_Throwable]

    New             _   -> CanThrow [java_lang_InstantiationError]
    ANew            _   -> CanThrow [java_lang_NegativeArraySizeException]                                                   
    MultiNew        _ _ -> CanThrow [java_lang_NegativeArraySizeException]

    ALength         -> CanThrow [java_lang_NullPointerException]
    Throw           -> AlwaysThrows [java_lang_Throwable] -- TODO: we can calculate it more accurately later

    CheckCast       _ -> CanThrow [java_lang_ClassCastException]
    InstanceOf      _ -> NoThrow

    MonitorEnter    -> CanThrow [java_lang_NullPointerException]
    MonitorExit     -> CanThrow [java_lang_NullPointerException, java_lang_IllegalMonitorStateException]
                    
    LookupSwitch    tbl def -> AlwaysBranches $ def:(map snd tbl)
    TableSwitch     _ _ offs def -> AlwaysBranches $ def:offs
    _                 -> NoThrow
    where
        isIntegral OpInt  = True
        isIntegral OpLong = True
        isIntegral _      = False
