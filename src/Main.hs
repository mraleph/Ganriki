module Main where

import Codec.Archive.Zip;
import qualified Data.ByteString.Lazy   as B;
import qualified Java.ClassParser.Class as JCP;
import Data.List;
import Control.Monad;

sources :: [FilePath]
sources = ["org.eclipse.osgi_3.4.0.v20080605-1900.jar", "EclipseLog.class"]

isClass :: FilePath -> Bool
isClass path = ".class" `isSuffixOf` path

isJar :: FilePath -> Bool
isJar path = ".jar" `isSuffixOf` path

load :: FilePath -> IO [JCP.Class]
load file | isJar file = do contents <- B.readFile file
                            let zipfile = toArchive contents
                            let entries = filter (isClass . eRelativePath) (zEntries zipfile)
                            let classes = map (JCP.parse . fromEntry) entries
                            return classes

load file | isClass file = do bytes <- B.readFile file
                              return [JCP.parse bytes]

main = do classes <- concat `liftM` mapM (load) sources
          let classloaders = filter ((=="java/lang/ClassLoader") . JCP.clsSuper) classes
          print classloaders





