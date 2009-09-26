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

vmopControlFlowType op = case op of
    GotoIf  _ target             -> CanBranch      [target]
    Goto    target               -> AlwaysBranches [target]
    Return  _                    -> End                    
    LookupSwitch    tbl def      -> AlwaysBranches $ def:(map snd tbl)
    TableSwitch     _ _ offs def -> AlwaysBranches $ def:offs
    _                            -> NoThrow

