module Main where

import System.Environment (getArgs)

import Java.ClassParser
import Ganriki.IR (translate, irCFG)
import Ganriki.GDL (toGDL)
import Control.Monad

import System.FilePath
import System.Directory

sources :: [FilePath]
sources = ["org.eclipse.osgi_3.4.0.v20080605-1900.jar", "EclipseLog.class"]

makeFileName m idx = do
    let filename = m ++ "~" ++ (show idx) ++ ".gdl"
    exists <- doesFileExist filename
    if exists then makeFileName m (idx+1) else return filename

main = do 
    sources <- getArgs
    ctx     <- loadAndLinkAll sources    
    let classes  = allClasses ctx
    let classes' = filter (`isSubClassOf` java_lang_ClassLoader) classes    
    let methods  = concat $ map clsMethods classes'
    forM_ methods $ \method -> do             
        putStrLn $ mName method
        case mCode method of
            Just code -> do putStrLn $ show method
                            let gdl = (toGDL $ irCFG $ translate code)
                            filename <- makeFileName (makeValid $ mName method) 0
                            writeFile filename gdl

            Nothing   -> putStrLn "\n(no code)"
