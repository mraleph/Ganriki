module Java.Types (JType(..), JString, JInt, JLong, JFloat, JDouble, JBits(..), toString) where

import Data.Word (Word8)
import Codec.Binary.UTF8.String (decode)

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
                 deriving (Show, Eq)

newtype JString = JString [Word8] deriving (Show)
newtype JInt    = JInt    [Word8] deriving (Show)
newtype JLong   = JLong   [Word8] deriving (Show)
newtype JFloat  = JFloat  [Word8] deriving (Show)
newtype JDouble = JDouble [Word8] deriving (Show)

class JBits a where
  fromBits :: [Word8] -> a

instance JBits JString where
  fromBits = JString

instance JBits JInt where
  fromBits = JInt

instance JBits JLong where
  fromBits = JLong

instance JBits JFloat where
  fromBits = JFloat

instance JBits JDouble where
  fromBits = JDouble

toString :: JString -> String
toString (JString s) = decode s
