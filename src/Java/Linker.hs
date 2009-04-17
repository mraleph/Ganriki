module Java.Linker (
   Class
,  clsName
,  clsSuper       
,  clsIfaces      
,  clsIsInterface 
,  clsMethods
,  Method, mAccess, mName, mSig, mCode
,  MethodCode, mcMaxStack, mcMaxLocals, mcCode, mcHandlers
,  ExceptionHandlerInfo (..)
,  LinkageContext
,  linkAll
,  resolve, tryResolve
,  allClasses
) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

import qualified Java.ClassParser.Class        as C
import qualified Java.ClassParser.Instructions as I

type Classes = M.Map String Class

data LinkageContext = Boot Classes
                    | Chain Classes LinkageContext

data Class = Class 
             {   clsName        :: String
             ,   clsSuper       :: Maybe Class
             ,   clsIfaces      :: [Class]
             ,   clsIsInterface :: Bool
             ,   clsMethods     :: [Method]                   
             }

data Method = Method 
              {   mAccess :: C.Access
              ,   mName   :: String
              ,   mSig    :: C.MethodSig
              ,   mCode   :: Maybe MethodCode
              } 

data MethodCode = MethodCode 
                  {   mcMaxStack  :: Int
                  ,   mcMaxLocals :: Int
                  ,   mcCode      :: I.Bytecode
                  ,   mcHandlers  :: [ExceptionHandlerInfo]
                  }

data ExceptionHandlerInfo = ExceptionHandlerInfo 
                            {   ehiStartPC   :: Int
                            ,   ehiEndPC     :: Int
                            ,   ehiHandlerPC :: Int
                            ,   ehiCatch     :: Maybe Class
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
                ,   clsMethods     = map (linkMethod ctx) $ C.clsMethods c
                }

linkMethod :: LinkageContext -> C.Method -> Method
linkMethod ctx m = Method 
                   {   mAccess = C.mAccess m
                   ,   mName   = C.mName m
                   ,   mSig    = C.mSig m
                   ,   mCode   = linkMethodCode ctx `fmap` C.mCode m
                   } 

linkMethodCode ctx code = MethodCode 
                          {   mcMaxStack  = C.mcMaxStack code
                          ,   mcMaxLocals = C.mcMaxLocals code
                          ,   mcCode      = C.mcCode code
                          ,   mcHandlers  = map (linkExceptionHandlerInfo ctx) $ C.mcHandlers code
                          }

linkExceptionHandlerInfo ctx ehi = ExceptionHandlerInfo 
                                   {   ehiStartPC   = C.ehiStartPC   ehi 
                                   ,   ehiEndPC     = C.ehiEndPC     ehi
                                   ,   ehiHandlerPC = C.ehiHandlerPC ehi
                                   ,   ehiCatch     = resolve ctx `fmap` C.ehiCatch ehi
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
