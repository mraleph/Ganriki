module Java.Linker (
   Class
,  clsName
,  clsSuper       
,  clsIfaces      
,  clsIsInterface 
,  clsMethods
,  LinkageContext
,  linkAll
,  resolve, tryResolve
,  allClasses
) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

import qualified Java.ClassParser.Class as C

type Classes = M.Map String Class

data LinkageContext = Boot Classes
                    | Chain Classes LinkageContext

data Class = Class { clsName        :: String
                   , clsSuper       :: Maybe Class
                   , clsIfaces      :: [Class]
                   , clsIsInterface :: Bool
                   , clsMethods     :: [C.Method]
                   }

tryResolve :: LinkageContext -> String -> Maybe Class

tryResolve (Boot cs)      classname = M.lookup classname cs
tryResolve (Chain cs ctx) classname = case M.lookup classname cs of
                                         Just c  -> Just c
                                         Nothing -> tryResolve ctx classname


resolve :: LinkageContext -> String -> Class

resolve (Boot cs) classname = case M.lookup classname cs of
                                  Just c  -> c
                                  Nothing -> error $ "Unable to find class " ++ classname
                                
resolve (Chain cs ctx) classname = case M.lookup classname cs of
                                       Just c  -> c
                                       Nothing -> resolve ctx classname
                            

linkOne :: LinkageContext -> C.Class -> Class
linkOne ctx c = Class 
                {   clsName        = C.clsName c                 
                ,   clsSuper       = resolve ctx `fmap` C.clsSuper c
                ,   clsIfaces      = map (resolve ctx) $ C.clsIfaces c
                ,   clsIsInterface = C.clsIsInterface c
                ,   clsMethods     = C.clsMethods c
                }

linkAll :: Maybe LinkageContext -> [C.Class] -> LinkageContext
linkAll pctx cs = ctx
    where classes = M.fromList $ map (\c -> (C.clsName c, linkOne ctx c)) cs
          ctx     = case pctx of 
                        Just p  -> Chain classes p
                        Nothing -> Boot classes

allClasses :: LinkageContext -> [Class]
allClasses (Boot cs)      = M.elems cs
allClasses (Chain cs ctx) = M.elems cs -- ++ allClasses ctx
