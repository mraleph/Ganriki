module Main where

import System.Environment (getArgs)

import Java.ClassParser
import Ganriki.D (translate)
import Control.Monad

sources :: [FilePath]
sources = ["org.eclipse.osgi_3.4.0.v20080605-1900.jar", "EclipseLog.class"]

main = do 
    sources <- getArgs
    classes <- allClasses `liftM` loadAndLinkAll sources    
    let classes' = filter (`isSubClassOf` java_lang_ClassLoader) classes    
    let methods  = concat $ map clsMethods classes'
    forM_ methods $ \method -> do             
        putStrLn $ mName method
        case mCode method of
            Just code -> do putStrLn $ show $ mcCode code
                            putStrLn $ show $ translate code
            Nothing   -> putStrLn "\n(no code)"
