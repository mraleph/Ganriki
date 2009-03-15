{-# LANGUAGE BangPatterns #-}

module Java.ClassParser.Class (Class (..), parse) where

import qualified Data.ByteString.Lazy as B;

import Data.Binary
import Data.Binary.Get

import Data.Array.ST
import Data.Array.Unboxed

import Control.Monad
import Data.Bits
import Data.Char
import Data.List

import Java.ClassParser.ConstantPool
import Java.Types

bToString :: B.ByteString -> String
bToString   = map (chr . fromIntegral) . B.unpack

bFromString :: String -> B.ByteString
bFromString = B.pack   . map (fromIntegral . ord)

onString :: (String -> String) -> (B.ByteString -> B.ByteString)
onString f  = bFromString . f . bToString

data Class = Class { clsName        :: JString
                   , clsIsInterface :: Bool
                   }

instance Show Class where
    show cls | clsIsInterface cls = "interface " ++ (toString $ clsName cls)
    show cls | otherwise          = "class "     ++ (toString $ clsName cls)

data Access = Public | Private | Protected | Package deriving (Eq)

instance Show Access where
    show Public = "public"
    show Private = "private"
    show Protected = "protected"
    show Package = ""

data Member = Method { mAccess :: Access, mName :: JString, mSig  :: MethodSig }
            | Field  { mAccess :: Access, mName :: JString, mType :: JType     }
            deriving (Show)


parse :: B.ByteString -> Class
parse = runGet classFileParser

-- The very class file parser

readByte :: Get Word8
readByte = get

readWord16 :: Get Word16
readWord16 = get

readInt16 :: Get Int
readInt16 = fromIntegral `liftM` readWord16

readWord32 :: Get Word32
readWord32 = get

classFileParser :: Get Class
classFileParser = do
    skip (4+2+2) -- magic, minor_version, major_version
    cpSize      <- readWord16
    cp          <- parseConstantPool $ (fromIntegral cpSize - 1)
    accessFlags <- readWord16
    thisClass   <- readInt16    
    let className      = (cpiName . cpEntry cp) thisClass
    let access         = flagsToAccess accessFlags
    let iface          = (0 /= accessFlags .&. 0x0200)
    skip 2                    -- super_class
    ifCount     <- readInt16  -- interfaces_count
    skip (2*ifCount)          -- u2 interfaces[interfaces_count]
    fieldCount  <- readInt16
    fields      <- replicateM fieldCount (parseMember cp Field parseFieldType)
    methodCount <- readInt16
    methods     <- replicateM methodCount (parseMember cp Method parseMethodSig)
    return (Class className iface)

parseMember :: ConstantPool                      ->
               (Access -> JString -> a -> Member) -> 
               (JString -> a)                    ->
               Get Member
parseMember cp ctor parser = do
    accessFlags <- readWord16
    nameIndex   <- readInt16
    descrIndex  <- readInt16
    attCount    <- readInt16
    replicateM attCount $ do skip 2; attLen <- readWord32; skip (fromIntegral attLen)
    return $ ctor (flagsToAccess accessFlags)
                  ((cpiStrValue . cpEntry cp) nameIndex)
                  ((parser . cpiStrValue . cpEntry cp) descrIndex)


flagsToAccess :: Word16 -> Access
flagsToAccess w | 0 /= w.&.0x0001 = Public
                | 0 /= w.&.0x0002 = Private
                | 0 /= w.&.0x0004 = Protected
                | otherwise       = Package

