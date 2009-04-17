
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment (getArgs)

import Java.ClassParser
import Ganriki.IR (translate, irCFG)
import Ganriki.GDL (toGDL)
import Control.Monad
import Data.List

import System.FilePath
import System.Directory
import System.IO
import System.Time

sources :: [FilePath]
sources = ["org.eclipse.osgi_3.4.0.v20080605-1900.jar", "EclipseLog.class"]

makeFileName m idx = do
    let filename = m ++ "~" ++ (show idx) ++ ".gdl"
    exists <- doesFileExist filename
    if exists then makeFileName m (idx+1) else return filename

toFile method code = do
    let gdl = (toGDL $ irCFG $ translate code)
    filename <- makeFileName (makeValid $ mName method) 0
    writeFile filename gdl

process !done !total (method:methods) = do
    begin <- getClockTime
    hPutStrLn stderr $ (show done) ++ "/" ++ (show total) ++ "\n"
    putStrLn $ mName method
    case mCode method of
        Just code -> putStrLn $ show $ irCFG $ translate code
        Nothing   -> putStrLn "\n(no code)"
    end <- getClockTime
    hPutStrLn stderr $ (mName method) ++ " took " ++ (show $ diffClockTimes end begin) ++ "\n"
    process (done+1) total methods

process _ _ [] = hPutStrLn stderr $ "done"

main = do 
    sources <- getArgs
    ctx     <- loadAndLinkAll sources    
    let classes  = allClasses ctx
    --let classes' = filter (`isSubClassOf` java_lang_ClassLoader) classes    
    let methods  = concat $ map clsMethods classes
    let nmethods = length methods
    process 0 nmethods methods    
