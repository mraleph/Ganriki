module Ganriki.VMControlFlow (targets) where

import Java.ClassParser

type PC = Int

data OpType = NoThrow
            | CanThrow
            | AlwaysThrows
            | CanBranch      [PC]
            | AlwaysBranches [PC]
            | End

targets :: MethodCode -> PC -> VMOp -> [PC]
targets code pc op =
    case opType op of
        NoThrow            -> [pc+1]
        CanThrow           -> (pc+1):handlers
        CanBranch      pcs -> (pc+1):pcs
        AlwaysThrows       -> handlers
        AlwaysBranches pcs -> pcs
        End                -> []
    where 
        handlers = map ehiHandlerPC $ filter (\h -> (ehiStartPC h <= pc) && (pc < ehiEndPC h)) $ mcHandlers code

opType :: VMOp -> OpType
opType op = case op of
    ALoad  _ -> CanThrow 
    AStore _ -> CanThrow
    Div    t | isIntegral t -> CanThrow
    Rem    t | isIntegral t -> CanThrow

    GotoIf  _ target -> CanBranch      [target]
    Goto    target   -> AlwaysBranches [target]

    JSR     _ -> error "JSR/Ret are not supported currently"
    Ret     _ -> error "JSR/Ret are not supported currently"

    Return  _ -> End

    GetStatic _ -> CanThrow
    PutStatic _ -> CanThrow
    GetField  _ -> CanThrow
    PutField  _ -> CanThrow

    InvokeSpecial   _ -> CanThrow
    InvokeStatic    _ -> CanThrow
    InvokeVirtual   _ -> CanThrow
    InvokeInterface _ -> CanThrow

    New             _ -> CanThrow
    ANew            _ -> CanThrow
    MultiNew        _ _ -> CanThrow

    ALength         -> CanThrow
    Throw           -> AlwaysThrows

    CheckCast       _ -> CanThrow
    InstanceOf      _ -> CanThrow

    MonitorEnter    -> CanThrow
    MonitorExit     -> CanThrow
                    
    LookupSwitch    tbl def -> AlwaysBranches $ def:(map snd tbl)
    TableSwitch     _ _ offs def -> AlwaysBranches $ def:offs
    _                 -> NoThrow
    where
        isIntegral OpInt  = True
        isIntegral OpLong = True
        isIntegral _      = False
