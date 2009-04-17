module Java.ClassParser (
    C.parse
    , Class, clsName, clsSuper, clsIfaces, clsIsInterface, clsMethods
    , Method, mAccess, mName, mSig, mCode
    , MethodCode, mcMaxStack, mcMaxLocals, mcCode, mcHandlers
    , ExceptionHandlerInfo (..)
    , VMOp(..), VMOperandType (..), bcCode, BranchCondition(..)
    -- platform classes
    , java_lang_ArrayIndexOutOfBoundsException, java_lang_NullPointerException, java_lang_Throwable
    , java_lang_ArithmeticException, java_lang_IncompatibleClassChangeError, java_lang_InstantiationError
    , java_lang_NegativeArraySizeException, java_lang_ClassCastException, java_lang_IllegalMonitorStateException, java_lang_NoSuchMethodError
    , java_lang_NoSuchFieldError, java_lang_LinkageError, java_lang_AbstractMethodError, java_lang_IllegalAccessError, java_lang_ClassLoader
    , LinkageContext, isSubClassOf, loadAndLinkAll, allClasses, resolve, tryResolve
    , CP.MethodRef, CP.mrClass, CP.mrName, CP.mrSig
    , CP.FieldRef, CP.frClass, CP.frName, CP.frType
    , CP.msParams, CP.msRetval
    , T.Constant (..), T.ConstantValue (..), T.JType (..)    
    , ArrayType, ArrayBaseType
) where

-- TODO move methods and related types to separate module
import qualified Java.Types as T
import qualified Java.ClassParser.Class as C
import qualified Java.ClassParser.ConstantPool as CP

import Java.ClassParser.Instructions
import Java.Linker

import Control.Monad.Reader
import qualified Data.Map as M (Map, fromList, lookup, elems)

import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy   as B
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)

import System.IO.Unsafe (unsafePerformIO) -- TODO may be we should use some beautiful mechanism?

------------------------------------------------------------------

isClass :: FilePath -> Bool
isClass path = ".class" `isSuffixOf` path

isJar :: FilePath -> Bool
isJar path = ".jar" `isSuffixOf` path

load :: FilePath -> IO [C.Class]
load file | isJar file = do zipfile  <- Z.toArchive file
                            let entries = filter (isClass . Z.eRelativePath) (Z.zEntries zipfile)
                            let classes = map (C.parse . Z.fromEntry) entries
                            return classes

load file | isClass file = do bytes <- B.readFile file                              
                              return [C.parse bytes]

loadAll :: [FilePath] -> IO [C.Class]
loadAll paths = concat `liftM` mapM (load) paths

{-# NOINLINE unsafeLoadAll #-}
unsafeLoadAll :: [FilePath] -> [C.Class]
unsafeLoadAll files = unsafePerformIO (loadAll files)

loadAndLinkAll files = linkAll (Just rtjar) `liftM` (loadAll files)

------------------------------------------------------------------

isSubClassOf :: Class -> Class -> Bool
isSubClassOf s p | clsName s == clsName p = True
                 | otherwise              = case clsSuper s of
                                                Just c  -> c `isSubClassOf` p 
                                                Nothing -> False

------------------------------------------------------------------

rtjar :: LinkageContext
rtjar = linkAll Nothing $ unsafeLoadAll ["rt.jar"]

platformClass name = case tryResolve rtjar name of
                         Just clazz -> clazz
                         Nothing    -> error $ "Unable to find platform class " ++ name

java_lang_ClassLoader :: Class
java_lang_ClassLoader = platformClass "java/lang/ClassLoader"

java_lang_ArrayIndexOutOfBoundsException :: Class
java_lang_ArrayIndexOutOfBoundsException = platformClass "java/lang/ArrayIndexOutOfBoundsException"

java_lang_NullPointerException :: Class
java_lang_NullPointerException = platformClass "java/lang/NullPointerException"

java_lang_Throwable :: Class    
java_lang_Throwable = platformClass "java/lang/Throwable"

java_lang_ArithmeticException :: Class
java_lang_ArithmeticException = platformClass "java/lang/ArithmeticException"

java_lang_IncompatibleClassChangeError :: Class
java_lang_IncompatibleClassChangeError = platformClass "java/lang/IncompatibleClassChangeError"

java_lang_InstantiationError :: Class
java_lang_InstantiationError = platformClass "java/lang/InstantiationError"

java_lang_NegativeArraySizeException :: Class
java_lang_NegativeArraySizeException = platformClass "java/lang/NegativeArraySizeException"

java_lang_ClassCastException :: Class
java_lang_ClassCastException = platformClass "java/lang/ClassCastException"

java_lang_IllegalMonitorStateException :: Class
java_lang_IllegalMonitorStateException = platformClass "java/lang/IllegalMonitorStateException"

java_lang_NoSuchMethodError :: Class   
java_lang_NoSuchMethodError = platformClass "java/lang/NoSuchMethodError"

java_lang_NoSuchFieldError :: Class   
java_lang_NoSuchFieldError = platformClass "java/lang/NoSuchFieldError"

java_lang_LinkageError :: Class   
java_lang_LinkageError = platformClass "java/lang/LinkageError"

java_lang_AbstractMethodError :: Class   
java_lang_AbstractMethodError = platformClass "java/lang/AbstractMethodError"

java_lang_IllegalAccessError :: Class   
java_lang_IllegalAccessError = platformClass "java/lang/IllegalAccessError"
