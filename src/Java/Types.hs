{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Java.Types (
    JType(..)
,   JString, readJString, toString
,   JInt, readJInt
,   JLong, readJLong
,   JFloat, readJFloat
,   JDouble, readJDouble
,   Constant (..)
,   ConstantValue (..)
) where

import Data.Word (Word8)
import qualified Codec.Binary.UTF8.String as UTF8

import Data.ByteString (unpack)

import Control.Monad (liftM)

import Data.Bits

import Data.Binary
import Data.Binary.Get

import Data.Int (Int32, Int64)

data JType       = TArray    { jtElementType :: JType  }
                 | TInstance { jtClassname   :: String }
                 | TByte
                 | TChar
                 | TDouble
                 | TFloat
                 | TInt
                 | TLong
                 | TShort
                 | TBoolean
                 deriving (Eq)

instance Show JType where
    show TByte    = "byte"
    show TChar    = "char"
    show TDouble  = "double"
    show TFloat   = "float"
    show TInt     = "int"
    show TLong    = "long"
    show TShort   = "short"
    show TBoolean = "boolean"
    show (TArray b) = (show b) ++ "[]"
    show (TInstance n) = n


newtype JString = JString String
newtype JInt    = JInt    Int32 deriving (Show)
newtype JLong   = JLong   Int64 deriving (Show)
newtype JFloat  = JFloat  Float deriving (Show)
newtype JDouble = JDouble Double deriving (Show)

data Constant = CNull
              | CString !String
              | CByte   !Int32
              | CShort  !Int32
              | CInt    !Int32
              | CLong   !Int64
              | CFloat  !Float
              | CDouble !Double

instance Show Constant where
    show v = case v of 
                 CNull     -> "null"
                 CString s -> "\"" ++ s ++ "\""
                 CByte   b -> show b
                 CShort  s -> show s
                 CInt    i -> show i
                 CLong   l -> (show l) ++ "L"
                 CFloat  f -> (show f) ++ "f"
                 CDouble d -> (show d) ++ "d"


instance Show JString where
    show s = toString s

readJInt :: Get JInt
readJInt = (JInt . fromIntegral) `liftM` getWord32be

readJFloat :: Get JFloat
readJFloat = do
    bits <- getWord32be
    return $ JFloat $ bits2float bits
    where bits2float bits | bits == 0x7f800000 = 1/0
          bits2float bits | bits == 0xff800000 = -1/0
          bits2float bits | ((0x7f800001 <= bits) && (bits <= 0x7fffffff)) || ((0xff800001 <= bits) && (bits <= 0xffffffff)) = 0/0
          bits2float bits | otherwise = let s :: Word32 = if (bits `shiftR` 31) == 0 then 1 else -1
                                            e :: Word32 = (bits `shiftR` 23) .&. 0xff
                                            m :: Word32 = if e == 0 then (bits .&. 0x7fffff) `shiftL` 1 else (bits .&. 0x7fffff) .|. 0x800000
                                        in encodeFloat (fromIntegral $ s * m) (fromIntegral $ e - 150)

readJLong :: Get JLong
readJLong = (JLong . fromIntegral) `liftM` getWord64be

readJDouble :: Get JDouble 
readJDouble = do
    bits <- getWord64be
    return $ JDouble $ bits2double bits
    where bits2double bits | bits == 0x7ff0000000000000 = 1/0
          bits2double bits | bits == 0xfff0000000000000 = -1/0
          bits2double bits | ((0x7ff0000000000001 <= bits) && (bits <= 0x7fffffffffffffff)) || ((0xfff0000000000001 <= bits) && (bits <= 0xffffffffffffffff)) = 0/0
          bits2double bits | otherwise = let s :: Word64 = if (bits `shiftR` 63) == 0 then 1 else -1
                                             e :: Word64 = (bits `shiftR` 52) .&. 0x7ff
                                             m :: Word64 = if e == 0 then (bits .&. 0xfffffffffffff) `shiftL` 1 else (bits .&. 0xfffffffffffff) .|.  0x10000000000000
                                         in encodeFloat (fromIntegral $ s * m) (fromIntegral $ e - 1075)


-- TODO: real JString has its own coding
readJString :: Int -> Get JString
readJString len = (JString . UTF8.decode . unpack) `liftM` (getBytes len)

toString :: JString -> String
toString (JString s) = s

class ConstantValue a where
    toConstant :: a -> Constant

instance ConstantValue JString where
    toConstant = CString . toString

instance ConstantValue JInt where
    toConstant (JInt i) = CInt i

instance ConstantValue JFloat where
    toConstant (JFloat f) = CFloat f

instance ConstantValue JLong where
    toConstant (JLong l) = CLong l

instance ConstantValue JDouble where
    toConstant (JDouble d) = CDouble d
