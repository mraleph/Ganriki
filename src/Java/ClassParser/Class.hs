{-# LANGUAGE BangPatterns #-}

module Java.ClassParser.Class (Class, parse, clsName, clsSuper, clsIfaces, clsIsInterface) where

import qualified Data.ByteString.Lazy as B

import Data.ByteString (unpack)

import Data.Binary
import Data.Binary.Get

import qualified Data.Map as M

import Data.Array.ST
import Data.Array.Unboxed

import Control.Monad
import Data.Bits
import Data.Char
import Data.List

import Java.ClassParser.ConstantPool
import Java.Types

import qualified Java.ClassParser.Instructions as I

bToString :: B.ByteString -> String
bToString   = map (chr . fromIntegral) . B.unpack

bFromString :: String -> B.ByteString
bFromString = B.pack   . map (fromIntegral . ord)

onString :: (String -> String) -> (B.ByteString -> B.ByteString)
onString f  = bFromString . f . bToString

data Class = Class { clsName        :: JString
                   , clsSuper       :: String
                   , clsIfaces      :: [String]
                   , clsIsInterface :: Bool
                   , clsMethods     :: [Method]
                   }

instance Show Class where
    show cls | clsIsInterface cls = "interface " ++ (toString $ clsName cls) ++ "\n" ++ (unlines $ map show $ clsMethods cls)
    show cls | otherwise          = "class "     ++ (toString $ clsName cls) ++ "\n" ++ (unlines $ map show $ clsMethods cls)

data Access = Public | Private | Protected | Package deriving (Eq)

instance Show Access where
    show Public = "public"
    show Private = "private"
    show Protected = "protected"
    show Package = ""

data ExceptionHandlerInfo = ExceptionHandlerInfo {
                                ehiStartPC   :: Int
                            ,   ehiEndPC     :: Int
                            ,   ehiHandlerPC :: Int
                            ,   ehiCatch     :: Maybe String
                            }

data MethodCode = MethodCode {
                      mcMaxStack  :: Int
                  ,   mcMaxLocals :: Int
                  ,   mcCode      :: [I.VMOp]
                  ,   mcHandlers  :: [ExceptionHandlerInfo]
                  }


data Method = Method { mAccess :: Access, mName :: JString, mSig  :: MethodSig, mCode       :: Maybe MethodCode } 
data Field  = Field  { fAccess :: Access, fName :: JString, fType :: JType, fAttributes :: Attributes } deriving (Show)

instance Show Method where
    show (Method a n s c) = "\t" ++ (toString n) ++ " " ++ (show s) ++ (maybe " = 0" (("\n" ++) . unlines . (map (("\t\t" ++) . show)) . mcCode) c)

type Attributes = M.Map String [Word8]

parse :: B.ByteString -> Class
parse = runGet classFileParser

readByte :: Get Word8
readByte = get

readWord16 :: Get Word16
readWord16 = get

readInt16 :: Get Int
readInt16 = fromIntegral `liftM` readWord16

readWord32 :: Get Word32
readWord32 = get

readInt32 :: Get Int
readInt32 = fromIntegral `liftM` readWord32

readBytes :: Int -> Get [Word8]
readBytes n = unpack `liftM` (getBytes n)

mkMethod :: ConstantPool -> Access -> JString -> MethodSig -> Attributes -> Method
mkMethod cp a n s attrs = 
    Method a n s $
        case M.lookup "Code" attrs of
            Nothing   -> Nothing
            Just code -> Just $ runGet (parseCode cp) (B.pack code)         

mkField :: ConstantPool -> Access -> JString -> JType -> Attributes -> Field
mkField cp = Field

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
    super       <- (toString . cpiName . cpEntry cp) `liftM` readInt16
    ifCount     <- readInt16
    ifaces      <- replicateM ifCount ((toString . cpiName . cpEntry cp) `liftM` readInt16)
    fieldCount  <- readInt16
    fields      <- replicateM fieldCount (parseMember cp mkField parseFieldType)
    methodCount <- readInt16
    methods     <- replicateM methodCount (parseMember cp mkMethod parseMethodSig)
    return (Class className super ifaces iface methods)

parseMember :: ConstantPool                                                     ->
               (ConstantPool -> Access -> JString -> a -> Attributes -> b)      ->
               (JString -> a)                                                   ->
               Get b

parseMember cp ctor parser = do
    accessFlags <- readWord16
    nameIndex   <- readInt16
    descrIndex  <- readInt16
    attrCount   <- readInt16
    attrs       <- parseAttributes cp attrCount
    return $ ctor cp
                  (flagsToAccess accessFlags)
                  ((cpiStrValue . cpEntry cp) nameIndex)
                  ((parser . cpiStrValue . cpEntry cp) descrIndex)
                  attrs


parseAttributes :: ConstantPool -> Int ->  Get Attributes
parseAttributes cp n =
    M.fromList `liftM` replicateM n (do idxName <- readInt16
                                        length  <- readInt32
                                        value   <- readBytes length
                                        return (toString $ cpiStrValue $ cpEntry cp idxName, value))

flagsToAccess :: Word16 -> Access
flagsToAccess w | 0 /= w.&.0x0001 = Public
                | 0 /= w.&.0x0002 = Private
                | 0 /= w.&.0x0004 = Protected
                | otherwise       = Package

parseCode :: ConstantPool -> Get MethodCode
parseCode cp = do
    maxStack       <- readInt16
    maxLocals      <- readInt16
    codeLength     <- readInt32
    bytecode       <- getBytes codeLength
    let code = runGet (I.parseCode cp) (B.pack $ unpack bytecode)
    handlersLenght <- readInt16
    handlers       <- replicateM handlersLenght $ liftM4 ExceptionHandlerInfo readInt16 readInt16 readInt16 $ do idx <- readInt16
                                                                                                                 if idx == 0 then return Nothing else return $ Just $ getClassName cp idx
    attrsCount     <- readInt16
    attrs          <- parseAttributes cp attrsCount -- TODO: currently discarded
    return $ MethodCode maxStack maxLocals code handlers



