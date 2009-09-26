{-# LANGUAGE ScopedTypeVariables #-}

module Obscuro (emitC) where

-- import Data.Graph.Inductive.Graph (ufold, lab', Context, node')

import qualified Java.ClassParser as J
import Ganriki.IR (IR(..), CFG, Op(..), BasicBlock (..), Local)

import Data.Maybe (catMaybes, fromJust)
import Data.List ( (\\), sort )


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph.Inductive.Graph as G

import Control.Monad.State

emitC :: J.Method -> IR -> IO ()
emitC m ir = do 
    let g      = irCFG ir
    let nodes  = G.labNodes g
    let ass    = execState (mapM_ (collector g) nodes) M.empty    
    let nodes' = map (removePhies ass) (nodes)
    let g' :: CFG     = G.mkGraph nodes' (G.labEdges g)
    putStrLn "goto L0;"
    mapM_ printVar $ M.toList $ infer m g'
    mapM_ putStrLn $ G.ufold (emitter g') [] g'
    -- putStrLn $ show cfg'

printVar (l, t) = putStrLn $ (typ2c t) ++ " l" ++ (show l) ++ ";"

infer :: J.Method -> CFG -> M.Map Local Typ
infer m g = eval constr [] M.empty
    where
        retval = typeof $ fromJust $ J.msRetval $ J.mSig m
        constr = G.ufold (infer' retval) [] g

infer' retval ctx m = foldl (infer'' retval) m $ fromBB $ G.lab' ctx

data Typ = TypObject
         | TypBoolean
         | TypByte
         | TypChar
         | TypShort
         | TypInt
         | TypFloat
         | TypLong 
         | TypDouble
         | TypEq Local
         | TypArray Typ
         | TypArrayOf Local
         | TypElementOf Local
         deriving (Show)

typ2c t = 
    case t of 
        TypBoolean          -> "jboolean"
        TypByte             -> "jbyte"
        TypChar             -> "jchar"
        TypShort            -> "jshort"
        TypInt              -> "jint"
        TypFloat            -> "jfloat"
        TypLong             -> "jlong"
        TypDouble           -> "jdouble"
        TypObject           -> "jobject" -- TODO [!!!]
        TypArray e          -> "jarray"  -- TODO [!!!]
        


type EvalState = M.Map Local Typ

eval :: [(Local, Typ)] -> [(Local, Typ)] -> EvalState -> EvalState
eval []     []  m = m
eval []     cx' m = eval cx' [] m
eval ((l, t):cx) cx' m = 
    case t of
        TypEq l'           -> 
            case M.lookup l' m of
                Just t' -> eval cx cx' $ M.insert l t' m         -- TODO [!!!] lookup if already inserted
                Nothing -> case M.lookup l m of
                               Just t' -> eval cx cx' $ M.insert l' t' m
                               Nothing -> eval cx ((l, t):cx') m
        TypElementOf l' ->
            case M.lookup l' m of
                Just (TypArray t') -> eval cx cx' $ M.insert l t' m         -- TODO [!!!] lookup if already inserted
                Nothing -> case M.lookup l m of
                               Just t' -> eval cx cx' $ M.insert l' (TypArray t') m
                               Nothing -> eval cx ((l, t):cx') m

        TypArrayOf l' ->
            case M.lookup l' m of
                Just t' -> eval cx cx' $ M.insert l (TypArray t') m         -- TODO [!!!] lookup if already inserted
                Nothing -> case M.lookup l m of
                               Just (TypArray t') -> eval cx cx' $ M.insert l' t' m
                               Nothing            -> eval cx ((l, t):cx') m
            
        _                  -> 
            eval cx cx' $ M.insert l t m         -- TODO [!!!] lookup if already inserted


class HasTyp a where
    typeof :: a -> Typ

instance HasTyp J.VMOperandType where
    typeof t = 
        case t of 
            J.OpRef      -> TypObject
            J.OpBoolean  -> TypBoolean
            J.OpByte     -> TypByte
            J.OpChar     -> TypChar
            J.OpShort    -> TypShort
            J.OpInt      -> TypInt
            J.OpFloat    -> TypFloat
            J.OpLong     -> TypLong 
            J.OpDouble   -> TypDouble

instance HasTyp J.Constant where
    typeof c = 
        case c of 
            J.CNull   -> TypObject
            J.CString _ -> TypObject
            J.CByte   _ -> TypByte
            J.CShort  _ -> TypShort
            J.CInt    _ -> TypInt
            J.CLong   _ -> TypLong
            J.CFloat  _ -> TypFloat
            J.CDouble _ -> TypDouble

instance HasTyp J.JType where
    typeof t = 
        case t of 
            J.TArray e    -> TypArray $ typeof e
            J.TInstance _ -> TypObject 
            J.TByte       -> TypByte
            J.TChar       -> TypChar
            J.TDouble     -> TypDouble
            J.TFloat      -> TypFloat
            J.TInt        -> TypInt
            J.TLong       -> TypLong
            J.TShort      -> TypShort
            J.TBoolean    -> TypBoolean
         

instance HasTyp J.ArrayBaseType where
    typeof t = 
        case J.abType t of 
            Left t' -> typeof t
            Right _ -> TypObject

instance HasTyp J.ArrayType where 
    typeof t =  (iterate TypArray $ typeof $ J.atBaseType t) !! (J.atDim t)
      



instance HasTyp J.FieldRef where
    typeof f = typeof $ J.frType f
            
instance HasTyp J.MethodRef where
    typeof m = typeof $ fromJust $ J.msRetval $ J.mrSig m

infer'' retval m op = 
    case op of
        Load l c                        -> l |= typeof c
        Assign l l'                     -> l |= TypEq l'
        Phi l l'                        -> settype' $ map (\x -> (l, TypEq x)) l'
        Invoke (Just l) obj mref args   -> settype' $ (obj, TypObject): (l, typeof mref): zip args (typeof' mref)
        Invoke Nothing  obj mref args   -> settype' $ (obj, TypObject): zip args (typeof' mref)
        InvokeStatic (Just l) mref args -> settype' $ (l, typeof mref): zip args (typeof' mref)
        InvokeStatic Nothing  mref args -> settype' $ zip args (typeof' mref)
        GetArray l arr idx              -> settype' [(arr, TypArrayOf l), (idx, TypInt), (l, TypElementOf arr)] 
        PutArray arr idx val            -> settype' [(arr, TypArrayOf val), (idx, TypInt), (val, TypElementOf arr)]
        PutField obj fref val           -> settype' [(obj, TypObject), (val, typeof fref)] 
        GetField l obj fref             -> settype' [(obj, TypObject), (l, typeof fref)] 
        PutStaticField fref l           -> l |= typeof fref
        GetStaticField l fref           -> l |= typeof fref
        Return (Just l)                 -> l |= retval
        Throw l             -> l |= TypObject
        Add  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        Sub  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        Mul  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        Div  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        Rem  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        Neg  l x            -> settype' [(l, TypEq x)]
        Shl  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        Shr  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        UShr l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        And  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        Or   l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        XOr  l x y          -> settype' [(l, TypEq x), (l, TypEq y), (x, TypEq y)]
        Coerce l to from l' -> settype' [(l, typeof to), (l', typeof from)]
        Cmp l x y          -> settype' [(l, TypBoolean), (x, TypEq y)]
        New l _            -> l |= TypObject
        MultiNew l t ls    -> settype' ((l, typeof t): map (\x -> (x, TypInt)) ls) -- TODO [!!!] possible error
        ANew l t sz        -> settype' [(l, TypArray $ typeof t), (sz, TypInt)]
        ALength l l'       -> settype' [(l, TypInt)] -- TODO polymorphic array operation
        CheckCast l _      -> l |= TypObject
        InstanceOf l l' _  -> settype' [(l,TypBoolean), (l',TypObject)]
        MonitorEnter l     -> l |= TypObject
        MonitorExit l      -> l |= TypObject
        LookupSwitch l _ _ -> l |= TypInt
        Catch l            -> l |= TypObject
        _                  -> m
    where
        l |= t = (l,t):m
        settype' ss = ss ++ m
        typeof' mref = undefined
        
        
    

-- emitter :: Context a b -> [String] -> [String]
emitter g ctx lst = ((label node) ++ ":") : (show $ G.lab' ctx) : exits' : "// -------------------------------------------" : lst
    where         
        lastop = head $ reverse $ fromBB $ G.lab' ctx
        node  = G.node' ctx
        exits = (G.suc g node) \\ (targets lastop)

         
        exits' = if alwaysBranches lastop
                 then ""
                 else
                     case exits of
                        []   -> "goto end;";
                        a:[] -> goto a
                        _    -> concatMap goto exits

        label l = "L" ++ (show l)
        
        goto l = "goto " ++ (label l) ++ ";"

        targets op =
            case op of                
                GotoIf _ _ _ pc           -> [pc]
                Goto pc                   -> [pc]                
                LookupSwitch _ cases dflt -> dflt : (map snd cases)
                _                         -> []

        alwaysBranches op =
            case op of                
                Goto _                    -> True
                LookupSwitch _ _ _        -> True
                Return _                  -> True
                _                         -> False

type PhiAssignments = M.Map G.Node (S.Set (Local, Local))

type Collector a = State PhiAssignments a

assign :: Local -> (G.Node, Local) -> Collector ()
assign l (n, l') = do
    m <- get
    let s = (l, l') `S.insert` (M.findWithDefault S.empty n m)        
    put $ M.insert n s m

collector :: CFG -> (G.Node, BasicBlock) -> Collector ()
collector g (n, bb) = mapM_ collect ops
    where
        ops = fromBB bb

        collect (Phi l ls) = mapM_ (assign l) $ zip (sort $ G.pre g n) ls
        collect _          = return ()

removePhies :: PhiAssignments -> (G.Node, BasicBlock) -> (G.Node, BasicBlock)
removePhies ass (n, bb) = (n, bb')
    where        
        ops'   = (filter (not . isPhie) (fromBB $ bb))
        (f, l) = splitAt ((length ops') - 1) ops' -- TODO: POSSIBLE BUG for short lists !!!
        bb'    = if isBranch l then BB (f ++ ass' ++ l) else BB (ops' ++ ass')

        isPhie (Phi _ _) = True
        isPhie _         = False

        isBranch [op] =
            case op of                
                GotoIf _ _ _ _            -> True
                Goto _                    -> True
                LookupSwitch _ _ _        -> True
                Return _                  -> True
                _                         -> False

        ass' = case M.lookup n ass of
                   Nothing -> []
                   Just s  -> map (uncurry Assign) (S.toList s)

