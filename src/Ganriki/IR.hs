{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Ganriki.IR (Op (..), IR(..), translate) where

import Control.Arrow (second)

import Util.QObject

import Data.Sequence ((|>))
import qualified Data.Sequence as S

import qualified Ganriki.VMControlFlow as VMControlFlow

import Ganriki.GDL (toGDL)

import Data.Int (Int32)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import Data.List (find, nub, sort, sortBy, mapAccumL, intercalate)
import Data.Foldable (toList)

import qualified Data.Set as Set
import qualified Data.Map as M

import qualified Java.ClassParser as J

import Data.Graph.Inductive (Gr, Node, suc, labNodes, labEdges, gmap, mkGraph, pre)
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.Dominators

import Control.Monad.State

type ClassRef = String
type Local    = Int

data BranchCondition = Eq
                     | Ne
                     | Lt
                     | Ge
                     | Gt
                     | Le
                     deriving (Show)

data Op  = Load !Local J.Constant 
         | Assign !Local !Local
         | Phi !Local [Local]
         | Invoke !(Maybe Local) !Local J.MethodRef ![Local]
         | InvokeStatic !(Maybe Local) J.MethodRef ![Local]
         | GetArray !Local !Local !Local
         | PutArray !Local !Local !Local
         | PutField !Local J.FieldRef !Local
         | GetField !Local !Local J.FieldRef
         | PutStaticField J.FieldRef !Local
         | GetStaticField !Local J.FieldRef
         | Return !(Maybe Local)
         | Throw !Local
         | Add  !Local !Local !Local
         | Sub  !Local !Local !Local
         | Mul  !Local !Local !Local
         | Div  !Local !Local !Local
         | Rem  !Local !Local !Local
         | Neg  !Local !Local
         | Shl  !Local !Local !Local
         | Shr  !Local !Local !Local
         | UShr !Local !Local !Local
         | And  !Local !Local !Local
         | Or   !Local !Local !Local
         | XOr  !Local !Local !Local
         | Coerce !Local !J.VMOperandType !J.VMOperandType !Local
         | Cmp !Local !Local !Local
         | New !Local ClassRef
         | MultiNew !Local !J.ArrayType ![Local]          
         | ANew !Local !J.ArrayBaseType !Local
         | ALength !Local !Local
         | CheckCast !Local !ClassRef
         | InstanceOf !Local !Local !ClassRef
         | MonitorEnter !Local                           
         | MonitorExit !Local
         | GotoIf BranchCondition !Local !Local Int
         | Goto Int
         | LookupSwitch !Local [(Int32, Int)] Int         
         | Catch !Local

newtype BasicBlock = BB { fromBB :: [Op] }

instance Show BasicBlock where
    show (BB ops) = intercalate "\n" $ map render ops

type CFG = Gr BasicBlock ()
data IR  = IR { irCFG :: CFG, irNLocals :: Int } deriving (Show)

fromJust' s Nothing  = error s
fromJust' s (Just x) =  x

-------------------------------------------------------------------------------
-- Translation to IR
-------------------------------------------------------------------------------

type PC      = Int
type Labeler = Int -> Int

data TranslationContext = Context 
                          { tcStackTop    :: !Int
                          , tcOps         :: S.Seq Op
                          , tcLabel       :: (Int -> Int)
                          }

type Translation a = State TranslationContext a

translate m = toSSA ir
  where 
      (ops, label) = translate'' m
      n            = length ops
      gates        = concatMap (\h -> [Catch (J.mcMaxLocals m), Goto $ label $ J.ehiHandlerPC h]) $ J.mcHandlers m
      redirect g h = (g + 2, h { J.ehiStartPC = label $ J.ehiStartPC h, J.ehiEndPC = label $ J.ehiEndPC h, J.ehiHandlerPC = g})
      ops'         = ops ++ gates
      ehi          = snd $ mapAccumL redirect n (J.mcHandlers m)
      cfg          = toGraph ehi ops'
      ir           = IR cfg (J.mcMaxLocals m + J.mcMaxStack m + 1)

translate'' :: J.MethodCode -> ([Op], Labeler)
translate'' m = (ops', label)
  where 
      ops       = J.mcCode m
      maxLocals = J.mcMaxLocals m

      (ops', labels) = translate' m label [(0, maxLocals)] M.empty
      label i   = fromJust' "label" $ M.lookup i labels

getOffset :: Translation Int
getOffset = (S.length . tcOps) `liftM` get

translate' :: J.MethodCode  -- method to translate
           -> (Int -> Int)  -- label translator
           -> [(PC, Int)]   -- worklist of locations to process with stack depth
           -> M.Map PC [Op] -- processed locations
           -> ([Op], M.Map Int Int)

translate' m label ((pc,depth):wl) ops = translate' m label (wl ++ wl') ops'
    where
        op = (J.bcCode $ J.mcCode m) !! pc
        s  = execState (translateOp op) (Context depth S.empty label)

        ops' = M.insert pc (toList $ tcOps s) ops
        wl'  = map (\(x, ex) -> (x, if ex then (J.mcMaxLocals m) + 1 else tcStackTop s)) $ filter (not . (`M.member` ops) . fst) (VMControlFlow.vmopTargets (J.mcHandlers m) pc op)
        


translate' _ _     []              ops = (concatMap snd ops', labels)
    where 
        ops' = M.assocs ops -- sortBy (\x y -> compare (fst x) (fst y)) ops          
        labels = M.fromList $ snd $ mapAccumL (\pc' (pc, l) -> (pc' + length l, (pc, pc'))) 0 ops'
          

translateOp :: J.VMOp -> Translation ()
translateOp op =
    case op of
        J.Push c    -> do i <- push $ typeof c
                          emit $ Load i c
        
        J.Load t l  -> do i <- push t
                          emit $ Assign i l

        J.Store t l -> do i <- pop t
                          emit $ Assign l i 

        J.ALoad t   -> do idx      <- pop J.OpInt
                          arrayref <- pop J.OpRef
                          value    <- push t
                          emit $ GetArray value arrayref idx

        J.AStore t  -> do value    <- pop t
                          idx      <- pop J.OpInt
                          arrayref <- pop J.OpRef                        
                          emit $ PutArray arrayref idx value


        J.Pop       -> 1 ==> []
        J.Pop2      -> 2 ==> []
        J.Dup       -> 1 ==> [1, 1]
        J.DupX1     -> 2 ==> [1, 2, 1]
        J.DupX2     -> 3 ==> [1, 3, 2, 1]
        J.Dup2      -> 2 ==> [2, 1, 2, 1]
        J.Dup2X1    -> 3 ==> [2, 1, 3, 2, 1]                     
        J.Dup2X2    -> 4 ==> [2, 1, 4, 3, 2, 1]
        J.Swap      -> 2 ==> [1, 2]

        J.Add t     -> binop t t t Add
        J.Sub t     -> binop t t t Sub
        J.Mul t     -> binop t t t Mul
        J.Div t     -> binop t t t Div
        J.Rem t     -> binop t t t Rem
        J.Neg t     -> unop t t Neg
        J.Shl t     -> binop t J.OpInt t Shl
        J.Shr t     -> binop t J.OpInt t Shr
        J.UShr t    -> binop t J.OpInt t UShr
        J.And t     -> binop t t t And
        J.Or  t     -> binop t t t Or
        J.XOr t     -> binop t t t XOr

        J.IInc l c  -> do tmp <- push J.OpInt
                          emit $ Load tmp (J.toConstant c)
                          emit $ Add l l tmp
                          pop' J.OpInt

        J.Coerce from to -> do vf <- pop from 
                               vt <- push to
                               emit $ Coerce vt from to vf 

        J.Cmp t      -> binop t t J.OpInt Cmp 

        J.GotoIf cond trg -> do l <- label trg
                                cmp cond l
                                

        J.Goto trg        -> label trg >>= (emit . Goto)

        J.JSR _ -> error "JSR/RET not supported"  
        J.Ret _ -> error "JSR/RET not supported"  
        
        J.Return (Just t)  -> do x <- pop t
                                 emit $ Return (Just x)

        J.Return (Nothing) -> do emit $ Return (Nothing)
                   
        J.GetStatic f -> do res <- push $ fieldType f
                            emit $ GetStaticField res f

        J.PutStatic f -> do value <- pop $ fieldType f
                            emit $ PutStaticField f value

        J.GetField f -> do obj <- pop J.OpRef
                           res <- push $ fieldType f
                           emit $ GetField res obj f
        
        J.PutField f -> do val <- pop $ fieldType f
                           obj <- pop J.OpRef
                           emit $ PutField obj f val

        J.InvokeSpecial m   -> do args <- reverse `liftM` mapM pop (reverse $ getSignature m)
                                  obj  <- pop J.OpRef 
                                  res  <- pushResult m
                                  emit $ Invoke res obj m args -- TODO: implement special lookup!

        J.InvokeStatic m    -> do args <- reverse `liftM` mapM pop (reverse $ getSignature m)
                                  res  <- pushResult m
                                  emit $ InvokeStatic res m args 

        J.InvokeVirtual m   -> do args <- reverse `liftM` mapM pop (reverse $ getSignature m)
                                  obj  <- pop J.OpRef 
                                  res  <- pushResult m
                                  emit $ Invoke res obj m args -- TODO: virtual lookup!

        J.InvokeInterface m -> do args <- reverse `liftM` mapM pop (reverse $ getSignature m)
                                  obj  <- pop J.OpRef 
                                  res  <- pushResult m
                                  emit $ Invoke res obj m args -- TODO: virtual interface lookup!

        J.New clazz  -> do x <- push J.OpRef
                           emit $ New x clazz

        J.ANew clazz -> do count <- pop J.OpInt
                           x     <- push J.OpRef
                           emit $ ANew x clazz count

        J.MultiNew t dims  -> do counts <- replicateM dims (pop J.OpInt)
                                 x      <- push J.OpRef
                                 emit $ MultiNew x t (reverse counts)


        J.ALength    -> do arrayref <- pop J.OpRef                         
                           res      <- push J.OpInt
                           emit $ ALength res arrayref

        J.Throw      -> do obj <- pop J.OpRef
                           emit $ Throw obj

        J.CheckCast clazz -> do obj <- top J.OpRef
                                emit $ CheckCast obj clazz

        J.InstanceOf clazz -> do obj <- pop J.OpRef
                                 res <- push J.OpBoolean
                                 emit $ InstanceOf res obj clazz

        J.MonitorEnter -> do obj <- pop J.OpRef
                             emit $ MonitorEnter obj                           
        
        J.MonitorExit -> do obj <- pop J.OpRef
                            emit $ MonitorExit obj                           
        
        
        J.LookupSwitch tbl def -> do key  <- pop $ J.OpInt
                                     tbl' <- fixtable tbl
                                     def'  <- label def
                                     emit $ LookupSwitch key tbl' def'
                
        J.TableSwitch low high tbl def  -> do key  <- pop $ J.OpInt
                                              tbl' <- fixtable $ zip [low..high] tbl
                                              def' <- label def
                                              emit $ LookupSwitch key tbl' def'
    where
        fixtable tbl = mapM (\(k, trg) -> label trg >>= \trg' -> return (k, trg')) tbl
            

sizeof :: J.VMOperandType -> Int
sizeof t = 
    case t of 
        J.OpRef      -> 1
        J.OpBoolean  -> 1
        J.OpByte     -> 1
        J.OpChar     -> 1
        J.OpShort    -> 1    
        J.OpInt      -> 1
        J.OpFloat    -> 1
        J.OpLong     -> 2        
        J.OpDouble   -> 2

push :: J.VMOperandType -> Translation Int
push t = do
    s <- get
    let idx    = tcStackTop s
    let newTop = sizeof t + idx
    put $ s { tcStackTop = newTop}
    return idx    

pop :: J.VMOperandType -> Translation Int
pop t = do
    s <- get
    let newTop = tcStackTop s - sizeof t
    put $ s { tcStackTop = newTop}
    return newTop

pop' :: J.VMOperandType -> Translation ()
pop' t = do
    s <- get
    let newTop = tcStackTop s - sizeof t
    put $ s { tcStackTop = newTop}

top :: J.VMOperandType -> Translation Int
top t = do 
    s <- get
    return $ tcStackTop s - sizeof t

typeof :: J.Constant -> J.VMOperandType
typeof c = 
    case c of 
        J.CNull     -> J.OpRef
        J.CString _ -> J.OpRef
        J.CByte   _ -> J.OpByte
        J.CShort  _ -> J.OpShort
        J.CInt    _ -> J.OpInt
        J.CLong   _ -> J.OpLong
        J.CFloat  _ -> J.OpFloat
        J.CDouble _ -> J.OpDouble

emit :: Op -> Translation ()
emit op = do
    s <- get
    put $ s { tcOps = tcOps s |> op }

(==>) :: Int -> [Int] -> Translation ()
nargs ==> after = do
    s <- get
    let t  = tcStackTop s
    let t' = t + diff
    put $ s { tcStackTop = t' } -- reserve space
    mapM_ emit (map (uncurry $ flip Assign) (assigns (t-1) (t'-1) t' (reverse after) []))
    where 
        diff    = length after - nargs
        assigns t t' tmp (a:ax) ass | alive t t' ax ass = (v, t')            : assigns t (t'-1) tmp     ax ((v, t'):ass) 
                                    | otherwise         = (t',tmp) : (v, t') : assigns t (t'-1) (tmp+1) ax ((t',tmp):(v, t'):ass) 
                                    where v = source (t-a+1) ass

        assigns t t' tmp []     ass                     = []

        alive t v (a:ax) ass | (v == t-a+1) = isJust $ lookup v ass -- value is required, check that it is alive somewhere
                             | otherwise    = alive t v ax ass
        alive t v []     ass                = True -- value will not be required

        source :: Int -> [(Int, Int)] -> Int
        source v ass = 
            case find ((v == ). snd) ass of
                Nothing -> v -- value was not corrupted
                Just _  -> fromJust' "copy of value" $ lookup v ass  -- value is corrupted, find copy

unop :: J.VMOperandType -> J.VMOperandType -> (Local -> Local -> Op) -> Translation ()
unop ta tres ctor = do
    a   <- pop  ta
    res <- push tres
    emit $ ctor res a

binop :: J.VMOperandType -> J.VMOperandType -> J.VMOperandType -> (Local -> Local -> Local -> Op) -> Translation ()
binop ta tb tres ctor = do
    b   <- pop  tb
    a   <- pop  ta
    res <- push tres
    emit $ ctor res a b

jtype2optype :: J.JType -> J.VMOperandType
jtype2optype t =
    case t of
        J.TArray _    -> J.OpRef
        J.TInstance _ -> J.OpRef
        J.TByte       -> J.OpByte
        J.TChar       -> J.OpChar
        J.TDouble     -> J.OpDouble
        J.TFloat      -> J.OpFloat
        J.TInt        -> J.OpInt
        J.TLong       -> J.OpLong
        J.TShort      -> J.OpShort
        J.TBoolean    -> J.OpBoolean
 

fieldType :: J.FieldRef -> J.VMOperandType
fieldType f = jtype2optype $ J.frType f

getSignature :: J.MethodRef -> [J.VMOperandType]
getSignature m = map (jtype2optype) $ J.msParams $ J.mrSig m

pushResult :: J.MethodRef -> Translation (Maybe Int)
pushResult m = 
    case J.msRetval $ J.mrSig m of
        Nothing -> return Nothing
        Just t  -> Just `liftM` push (jtype2optype t)

label :: Int -> Translation Int
label i = do
  s <- get
  return $ (tcLabel s) i

cmp :: J.BranchCondition -> Int -> Translation ()
cmp cond l = 
    case cond of 
        J.ZEq     -> ccmp J.Eq (J.CInt 0)
        J.ZNe     -> ccmp J.Ne (J.CInt 0)
        J.ZLt     -> ccmp J.Eq (J.CInt 0)
        J.ZGe     -> ccmp J.Ge (J.CInt 0)
        J.ZGt     -> ccmp J.Gt (J.CInt 0)
        J.ZLe     -> ccmp J.Le (J.CInt 0)
        J.Null    -> ccmp J.Eq J.CNull
        J.NotNull -> ccmp J.Ne J.CNull
        J.RefEq   -> cmp J.Eq l
        J.RefNe   -> cmp J.Ne l
        J.Eq      -> cmp' Eq
        J.Ne      -> cmp' Ne
        J.Lt      -> cmp' Lt
        J.Ge      -> cmp' Ge
        J.Gt      -> cmp' Gt
        J.Le      -> cmp' Le
    where 
        ccmp :: J.BranchCondition -> J.Constant -> Translation ()
        ccmp cond c = do 
            tmp <- push $ J.OpInt
            emit $ Load tmp c
            cmp cond l

        cmp' :: BranchCondition -> Translation ()
        cmp' cond = do
            val2 <- pop $ J.OpInt
            val1 <- pop $ J.OpInt
            emit $ GotoIf cond val1 val2 l

-------------------------------------------------------------------------------
-- Graph Conversion 
-------------------------------------------------------------------------------


-- TODO: respect throw handlers

toGraph :: [J.ExceptionHandlerInfo] -> [Op] -> CFG
toGraph ehi ops = mkGraph basicblocks branches
    where            
        splits = sort $ nub $ 0:n:(concatMap (targets') $ zip ops [0..(n-1)])

        build (s:e:xl) ops = ((s, take (e-s) ops), map (\t -> (s, t, ())) $ filter (< n) $ targets ehi (e-1) (head ops')): build (e:xl) (tail ops') where ops' = drop (e-s-1) ops
        build _ ops        = []

        basicblocks = map ((second BB) . fst) $ build splits ops
        branches    = concatMap snd $ build splits ops

        targets' (op, pc) = 
            case ts of
                []                  -> [pc+1] -- pc is split point
                [pc'] | pc' == pc+1 -> []   -- part of basic block it seems
                _                   -> ts   -- split point to
            where ts = nub $ targets ehi pc op
                  

        n = length ops
        

targets :: [J.ExceptionHandlerInfo] -> PC -> Op -> [PC]
targets ehi pc op = map fst $ VMControlFlow.targets ehi pc (iropControlFlowType op)

-- NOTE: we assume that: 
--   1) all classes are resolved and initialized eagerly before method execution 
--          => there will be no linking errors
--   2) there is infinite amount of memory :)
--          => no out of memory exceptions

iropControlFlowType :: Op -> VMControlFlow.OpType
iropControlFlowType op = case op of
    GetArray _ _ _ -> VMControlFlow.CanThrow [J.java_lang_ArrayIndexOutOfBoundsException, J.java_lang_NullPointerException]
    PutArray _ _ _ -> VMControlFlow.CanThrow [J.java_lang_ArrayIndexOutOfBoundsException, J.java_lang_NullPointerException]
    Div _ _ _      -> VMControlFlow.CanThrow [J.java_lang_ArithmeticException] -- TODO: lost type information
    Rem _ _ _      -> VMControlFlow.CanThrow [J.java_lang_ArithmeticException] -- TODO: lost type information

    GotoIf _ _  _ target -> VMControlFlow.CanBranch      [target]
    Goto target          -> VMControlFlow.AlwaysBranches [target]

    Return  _ -> VMControlFlow.End
    
    GetStaticField _ _ -> VMControlFlow.NoThrow
    PutStaticField _ _ -> VMControlFlow.NoThrow
    GetField  _ _ _    -> VMControlFlow.CanThrow [J.java_lang_NullPointerException]
    PutField  _ _ _    -> VMControlFlow.CanThrow [J.java_lang_NullPointerException]

    Invoke  _ _ _ _    -> VMControlFlow.CanThrow [J.java_lang_Throwable]
    InvokeStatic _ _ _ -> VMControlFlow.CanThrow [J.java_lang_Throwable]


    New             _ _   -> VMControlFlow.CanThrow [J.java_lang_InstantiationError]
    ANew            _ _ _ -> VMControlFlow.CanThrow [J.java_lang_NegativeArraySizeException]                                                   
    MultiNew        _ _ _ -> VMControlFlow.CanThrow [J.java_lang_NegativeArraySizeException]

    ALength _ _  -> VMControlFlow.CanThrow [J.java_lang_NullPointerException]
    Throw _      -> VMControlFlow.AlwaysThrows [J.java_lang_Throwable] -- TODO: we can calculate it more accurately later

    CheckCast   _ _ -> VMControlFlow.CanThrow [J.java_lang_ClassCastException]
    InstanceOf  _ _ _ -> VMControlFlow.NoThrow

    MonitorEnter _   -> VMControlFlow.CanThrow [J.java_lang_NullPointerException]
    MonitorExit  _   -> VMControlFlow.CanThrow [J.java_lang_NullPointerException, J.java_lang_IllegalMonitorStateException]
                    
    LookupSwitch  _ tbl def -> VMControlFlow.AlwaysBranches $ def:(map snd tbl)    
    _                 -> VMControlFlow.NoThrow

-------------------------------------------------------------------------------
-- SSA Conversion 
-------------------------------------------------------------------------------

type NodeSet = Set.Set Node

type RenamingMap   = M.Map Local Local
data RenamingState = RenamingState 
                     {   rsFreeLocal   :: Local -- number of first free local number
                     ,   rsActive      :: RenamingMap
                     ,   rsActiveStack :: [RenamingMap]
                     ,   rsActiveMaps  :: [(Node, RenamingMap)]
                     ,   rsPhies       :: [(Node, [(Local, Local)])]
                     ,   rsAlive       :: Set.Set Local
                     }

type Renaming a = State RenamingState a

toSSA :: IR -> IR
toSSA ir = ir'
    where 
        g = irCFG ir

        idoms = iDom g 0

        nodes = labNodes g

        idom :: Node -> Node -> Bool
        idom x y = {-# SCC "idom" #-}
            case lookup y idoms of
                Nothing -> False
                Just x' -> x == x'

        childrens :: M.Map Node [Node]
        childrens = M.fromList $ map (\x -> (x, children' x)) (G.nodes g)
            where children' x = (map fst $ filter (\(y, x') -> x' == x) idoms)

        children :: Node -> [Node]
        children x = {-# SCC "children" #-} fromJust $ M.lookup x childrens -- map fst $ filter (\(y, x') -> x' == x) idoms

        df :: Node -> [Node]
        df x = {-# SCC "df" #-} nub (filter (not . (x `idom`)) (suc g x) ++ concatMap (\z -> filter (not . (x `idom`)) (df z)) (children x))

        fixpoint :: NodeSet -> (NodeSet -> NodeSet) -> NodeSet
        fixpoint x f = {-# SCC "df" #-} let x' = (f x) in if x' /= x then fixpoint x' f else x'

        df0 :: NodeSet -> NodeSet
        df0 s = {-# SCC "df0" #-} Set.fold (\x s' -> (Set.fromList $ df x) `Set.union` s') Set.empty s

        idf :: NodeSet -> NodeSet
        idf s = {-# SCC "idf" #-} fixpoint (df0 s) (\s' -> df0 (s `Set.union` s'))

        assignments :: Local -> NodeSet
        assignments l = Set.fromList $ map fst $ filter (\(_, BB ops) -> (isAssignmentTo l) `any` ops) nodes

        phies :: Local -> NodeSet
        phies l = idf (assignments l)        

        allPhies = map (\l -> (l, phies l)) [0 .. (irNLocals ir)]

        phiesIn n = {-# SCC "phiesIn" #-} map fst $ filter (\(_, phs) -> n `Set.member` phs) allPhies

        -- g'  = gmap (\(preds, n, BB ops, succs) -> (preds, n, BB $ (phiesForNode (length $ pre g n) n) ++ ops, succs)) g

        g' = mkGraph (map insertPhies nodes') (labEdges g)

        ir' = IR g' (irNLocals ir)

        cleanMap = M.fromList $ zip [0 .. (irNLocals ir)] [0 .. (irNLocals ir)]

        (nodes', rs) = runState (rename 0) $ RenamingState { rsFreeLocal   = irNLocals ir + 1
                                                           , rsActive      = cleanMap
                                                           , rsActiveStack = []
                                                           , rsActiveMaps  = []
                                                           , rsPhies       = []
                                                           , rsAlive       = Set.empty
                                                           }

        insertPhies :: (Node, BasicBlock) -> (Node, BasicBlock)
        insertPhies node@(n, BB ops) = 
            case lookup n (rsPhies rs) of
                Nothing    -> node
                Just phies -> (n, BB $ (mapMaybe mkPhie phies) ++ ops)
            where
                mkPhie (l, l') | l' `Set.member` (rsAlive rs) = Just $ Phi l' (map (which l) $ sort $ pre g n)
                               | otherwise                  = Nothing

                which l n      = fromJust' "which-1" $ M.lookup l (fromJust' "which-2" $ lookup n (rsActiveMaps rs))

        
        getBlock n = fromBB $ fromJust $ lookup n nodes

        rename :: Node -> Renaming [(Node, BasicBlock)]
        rename n = do
            enter n                
            ops' <- catMaybes `liftM` mapM renameOp (getBlock n)
            rest <- concat    `liftM` mapM rename (children n)
            leave n
            return $ (n, BB $ ops'):rest


        enter :: Node -> Renaming ()
        enter n = do
            s <- get

            let (free', renaming) = mapAccumL (\l' l -> (l'+1, (l, l'))) (rsFreeLocal s) (phiesIn n)
            let active            = M.fromList $ renaming

            put $ s { rsFreeLocal   = free'
                    , rsActive      = active `M.union` (rsActive s)
                    , rsActiveStack = (rsActive s):(rsActiveStack s)
                    , rsPhies       = if renaming /= [] then (n, renaming):(rsPhies s) else rsPhies s
                    }

        
        leave :: Node -> Renaming ()
        leave n = do
            s <- get
            let (m:mx) = rsActiveStack s
            put $ s { rsActive = m, rsActiveStack = mx, rsActiveMaps = (n, rsActive s):(rsActiveMaps s) }

        version :: Local -> Renaming Local
        version l = do
            s <- get
            let l' = rsFreeLocal s
            put $ s { rsFreeLocal = l' + 1, rsActive = M.insert l l' (rsActive s) }
            return l'

        active :: Local -> Renaming Local
        active l = do
            s <- get
            let l' = M.findWithDefault l l (rsActive s)
            put $ s { rsAlive = Set.insert l' (rsAlive s) }
            return l'

        renameOp :: Op -> Renaming (Maybe Op)
        renameOp (Assign l r) = do            
            s <- get
            let r' = M.findWithDefault r r (rsActive s)
            put $ s { rsActive = M.insert l r' (rsActive s) }            
            return Nothing -- consume assigment            

        renameOp op           = Just `liftM` renameOp' op
            
        renameOp' :: Op -> Renaming Op
        renameOp' op = 
            case op of
                Load l c                      -> liftM  (\l'             -> Load l' c)                      (version l)
                -- Assign l r                    -> assign l r  -- liftM2 (\r' l'          -> Assign l' r')                   (active r) (version l)
                Invoke (Just l)  o m args     -> liftM3 (\o' args' l'    -> Invoke (Just l') o' m args')    (active o) (mapM active args) (version l)
                Invoke (Nothing) o m args     -> liftM2 (\o' args'       -> Invoke (Nothing) o' m args')    (active o) (mapM active args)
                InvokeStatic (Just l)  m args -> liftM2 (\args' l'       -> InvokeStatic (Just l') m args') (mapM active args) (version l)
                InvokeStatic (Nothing) m args -> liftM  (\args'          -> InvokeStatic (Nothing) m args') (mapM active args)
                GetArray l arr idx            -> liftM3 (\arr' idx' l'   -> GetArray l' arr' idx')          (active arr) (active idx) (version l)
                PutArray arr idx val          -> liftM3 (\arr' idx' val' -> PutArray arr' idx' val')        (active arr) (active idx) (active val)
                PutField o f val              -> liftM2 (\o' val'        -> PutField o' f val')             (active o) (active val)
                GetField l o f                -> liftM2 (\o' l'          -> GetField l' o' f)               (active o) (version l)
                PutStaticField f val          -> liftM  (\val'           -> PutStaticField f val')          (active val)
                GetStaticField l f            -> liftM  (\l'             -> GetStaticField l' f)            (version l)
                Return (Just l)               -> liftM  (\l'             -> Return (Just l'))               (active l)
                Throw l                       -> liftM  (\l'             -> Throw l')                       (active l)
                Neg  l x                      -> liftM2 (\x' l'          -> Neg l' x')                      (active x) (version l)
                Coerce l f t x                -> liftM2 (\x' l'          -> Coerce l' f t x')               (active x) (version l)
                New l clazz                   -> liftM  (\l'             -> New l' clazz)                   (version l)
                MultiNew l t counts           -> liftM2 (\counts' l'     -> MultiNew l' t counts')          (mapM active counts) (version l)
                ANew l t cnt                  -> liftM2 (\cnt' l'        -> ANew l' t cnt')                 (active cnt) (version l)
                ALength l arr                 -> liftM2 (\arr' l'        -> ALength l' arr')                (active arr) (version l)
                CheckCast l clazz             -> liftM  (\l'             -> CheckCast l' clazz)             (active l)
                InstanceOf l obj clazz        -> liftM2 (\l' obj'        -> InstanceOf l' obj' clazz)       (active obj) (version l)
                MonitorEnter l                -> liftM  (\l'             -> MonitorEnter l')                (active l)
                MonitorExit l                 -> liftM  (\l'             -> MonitorExit l')                 (active l)
                LookupSwitch l tbl def        -> liftM  (\l'             -> LookupSwitch l' tbl def)        (active l)
                Catch l                       -> liftM  (\l'             -> Catch l')                       (version l)
                Add  l x y                    -> binop' Add  l x y
                Sub  l x y                    -> binop' Sub  l x y
                Mul  l x y                    -> binop' Mul  l x y
                Div  l x y                    -> binop' Div  l x y
                Rem  l x y                    -> binop' Rem  l x y
                Shl  l x y                    -> binop' Shl  l x y
                Shr  l x y                    -> binop' Shr  l x y
                UShr l x y                    -> binop' UShr l x y
                And  l x y                    -> binop' And  l x y
                Or   l x y                    -> binop' Or   l x y
                XOr  l x y                    -> binop' XOr  l x y
                Cmp l x y                     -> binop' Cmp  l x y
                
                Phi l args                    -> error "Got Phi() in non SSA IR"

                _                             -> return op
            where 
                binop' ctor l x y = liftM3 (\x' y' l' -> ctor l' x' y') (active x) (active y) (version l)


        isAssignmentTo l op = 
            case op of 
                Load l' _   -> l' == l
                Assign l' _ -> l' == l
                Phi l' _ -> l' == l
                Invoke (Just l') _ _ _ -> l' == l
                InvokeStatic (Just l') _ _ -> l' == l
                GetArray l' _ _ -> l' == l
                GetField l' _ _ -> l' == l
                GetStaticField l' _ -> l' == l
                Add  l' _ _ -> l' == l
                Sub  l' _ _ -> l' == l
                Mul  l' _ _ -> l' == l
                Div  l' _ _ -> l' == l
                Rem  l' _ _ -> l' == l
                Neg  l' _ -> l' == l
                Shl  l' _ _ -> l' == l
                Shr  l' _ _ -> l' == l
                UShr l' _ _ -> l' == l
                And  l' _ _ -> l' == l
                Or   l' _ _ -> l' == l
                XOr  l' _ _ -> l' == l
                Coerce l' _ _ _ -> l' == l
                Cmp l' _ _ -> l' == l
                New l' _ -> l' == l
                MultiNew l' _ _ -> l' == l
                ANew l' _ _ -> l' == l
                ALength l' _ -> l' == l
                CheckCast l' _ -> l' == l
                InstanceOf l' _ _ -> l' == l
                Catch l' -> l' == l
                _ -> False

-------------------------------------------------------------------------------------------------------

instance QObject J.Constant where
    render x = show x

instance QObject BranchCondition where
    render x = 
        case x of 
            Eq -> "=="
            Ne -> "!="
            Lt -> "<"
            Ge -> "=>"
            Gt -> ">"
            Le -> "<"

instance QObject Op where
    render op = 
        case op of
            Load l c                      -> (local l) ++ " = " ++ (render c)
            Assign l r                    -> (local l) ++ " = " ++ (local r)
            Phi l args                    -> (local l) ++ " = " ++ "phi " ++ locals args
            Invoke (Just l)  o m args     -> (local l) ++ " = " ++ (local o) ++ "." ++ (J.mrName m) ++ locals args
            Invoke (Nothing) o m args     -> (local o) ++ "." ++ (J.mrName m) ++ locals args
            InvokeStatic (Just l)  m args -> (local l) ++ " = " ++ (J.mrName m) ++ locals args -- TODO: short classname
            InvokeStatic (Nothing) m args -> (J.mrName m) ++ locals args -- TODO: short classname
            GetArray l arr idx            -> (local l) ++ " = " ++ (local arr) ++ "[" ++ (local idx) ++ "]"
            PutArray arr idx val          -> (local arr) ++ "[" ++ (local idx) ++ "] = " ++ (local val) 
            PutField o f val              -> (local o) ++ "." ++ (J.frName f) ++ " = " ++ (local val) 
            GetField l o f                -> (local l) ++ " = " ++  (local o) ++ "." ++ (J.frName f)
            PutStaticField f val          -> (J.frName f) ++ " = " ++ (local val) -- TODO: short classname
            GetStaticField l f            -> (local l) ++ " = " ++ (J.frName f)-- TODO: short classname
            Return (Just l)               -> "return " ++ (local l)
            Return (Nothing)              -> "return"
            Throw l                       -> "thow" ++ (local l)
            Add  l x y                    -> rbinop "+" l x y
            Sub  l x y                    -> rbinop "-" l x y
            Mul  l x y                    -> rbinop "*" l x y
            Div  l x y                    -> rbinop "/" l x y
            Rem  l x y                    -> rbinop "%" l x y
            Shl  l x y                    -> rbinop "<<" l x y
            Shr  l x y                    -> rbinop ">>" l x y
            UShr l x y                    -> rbinop ">>>" l x y
            And  l x y                    -> rbinop "&" l x y
            Or   l x y                    -> rbinop "|" l x y
            XOr  l x y                    -> rbinop "^" l x y
            Neg  l x                      -> (local l) ++ " = -" ++ (local x)
            Coerce l _ t v                -> (local l) ++ " = (" ++ (render t) ++ ")" ++ (local v)
            Cmp l x y                     -> rbinop "<cmp>" l x y
            New l clazz                   -> (local l) ++ " = new " ++ clazz      ++ "()"
            MultiNew l t cnts             -> (local l) ++ " = new " ++ (render t) ++ "(" ++ (locals cnts) ++ ")"
            ANew l base cnt               -> (local l) ++ " = new " ++ (render base) ++ "[" ++ (local cnt) ++ "]"
            ALength l arr                 -> (local l) ++ " = " ++ (local arr) ++ ".length"
            CheckCast l clazz             -> "check " ++ (local l) ++ " is " ++ clazz
            InstanceOf l obj clazz        -> (local l) ++ " = " ++ (local obj) ++ " instanceof " ++ clazz
            MonitorEnter l                -> "lock (" ++ (local l) ++ ")"
            MonitorExit l                 -> "unlock (" ++ (local l) ++ ")"
            GotoIf cond x y t             -> "if " ++ (local x) ++ " " ++ (render cond) ++ " " ++ (local y) ++ " then goto " ++ (show t)
            Goto t                        -> "goto " ++ (show t)
            LookupSwitch l tbl def        -> "switch " ++ (local l) ++ "\n\t" ++ (showtbl tbl) ++ "\n\tdefault -> " ++ (show def)
            Catch l                       -> (local l) ++ " = @xobj"
        where
            showtbl tbl = intercalate "\n\t" $ map (\(l, t) -> ( (show l) ++ " -> " ++ (show t))) tbl
            local l   = "l" ++ (show l)
            locals ls = "(" ++ (intercalate ", " $ map local ls) ++ ")"
            rbinop op l x y = (local l) ++ " = " ++ (local x) ++ " " ++ op ++ " " ++ (local x)