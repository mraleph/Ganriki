module Java.ClassParser.ConstantPool (
    ConstantPool
,   cpEntry
,   ConstantPoolInfo
,   parseConstantPool
,   MethodSig
,   msParams
,   msRetval
,   cpiName
,   cpiClass
,   cpiType
,   cpiSig
,   cpiStrValue
,   cpiIntValue
,   cpiFloatValue
,   cpiLongValue
,   cpiDoubleValue
,   parseFieldType
,   parseMethodSig
) where

import Java.Types

import Control.Monad (liftM, forM)
import Data.Word (Word8, Word16)
import Data.Int (Int64)
import Data.ByteString (unpack)
import qualified Data.ByteString.Lazy as L (ByteString, take, unpack)

import Data.List (unfoldr)
import Data.Maybe (fromJust)

import Data.Binary
import Data.Binary.Get

-- introduce correction, after long/double must follow emtpy CNone
-- also prepend CNone
newtype ConstantPool = ConstantPool { cpEntries :: [ConstantPoolInfo] }

data MethodSig  = MethodSig  { msParams :: [JType], msRetval :: Maybe JType            } deriving (Show)

data ConstantPoolInfo = CClass           { cpiName  :: JString                                            }
                      | CField           { cpiClass :: JString, cpiName :: JString, cpiType :: JType      }
                      | CMethod          { cpiClass :: JString, cpiName :: JString, cpiSig  :: MethodSig  }
                      | CInterfaceMethod { cpiClass :: JString, cpiName :: JString, cpiSig  :: MethodSig  }
                      | CString          { cpiStrValue    :: JString                                      }
                      | CInteger         { cpiIntValue    :: JInt                                         }
                      | CFloat           { cpiFloatValue  :: JFloat                                       }
                      | CLong            { cpiLongValue   :: JLong                                        }
                      | CDouble          { cpiDoubleValue :: JDouble                                      }
                      | CNameAndType     { cpiName :: JString, cpiDesc :: Either JType MethodSig          }
                      | CUTF8            { cpiStrValue    :: JString                                      }
                      | CNone
                      deriving (Show)

parseConstantPool :: Int -> Get ConstantPool
parseConstantPool n = do s <- getRemainingLazyByteString
                         let (cpSize, cp) = parse' n s
                         uncheckedSkip cpSize
                         return cp

parse' :: Int -> L.ByteString -> (Int64, ConstantPool)
parse' n s = (cpSize, cp)
   where getEntries pos = do entry <- parseEntry cp pos
                             let newpos    = if isDoubleEntry entry then pos + 2 else pos + 1
                             let entries x = if isDoubleEntry entry then entry:CNone:x else entry:x
                             if newpos >= n then return $ entries []
                                            else entries `liftM` getEntries newpos

         (cp, _, cpSize) = runGetState ((ConstantPool . (CNone:)) `liftM` getEntries 0) s 0

         isDoubleEntry (CLong _)   = True
         isDoubleEntry (CDouble _) = True
         isDoubleEntry _           = False


parseEntry :: ConstantPool -> Int -> Get ConstantPoolInfo
parseEntry cp i = do
    tag <- readByte
    case tag of
        1  -> do len <- readInt16  -- CONSTANT_Utf8_info
                 (CUTF8 . fromBits) `liftM` readBytes len
        2  -> error "Tag 2 is unused"
        3  -> (CInteger . fromBits)                `liftM` readBytes 4 -- CONSTANT_Integer_info
        4  -> (CFloat . fromBits)                  `liftM` readBytes 4 -- CONSTANT_Float_info
        5  -> (CLong . fromBits)                   `liftM` readBytes 8 -- CONSTANT_Long_info
        6  -> (CDouble . fromBits)                 `liftM` readBytes 8 -- CONSTANT_Double_info
        7  -> (CClass . cpiStrValue . cpEntry cp)  `liftM` readInt16  -- CONSTANT_Class_info
        8  -> (CString . cpiStrValue . cpEntry cp) `liftM` readInt16 -- CONSTANT_String_info
        9  -> do clazz <- readInt16            -- CONSTANT_Fieldref_info
                 nat   <- readInt16
                 return $ mkFieldInfo (cpiName $ cpEntry cp clazz) (cpEntry cp nat)
        10 -> do clazz <- readInt16            -- CONSTANT_Methodref_info
                 nat   <- readInt16
                 return $ mkMethodInfo (cpiName $ cpEntry cp clazz) (cpEntry cp nat)
        11 -> do clazz <- readInt16            -- CONSTANT_InterfaceMethodref_info
                 nat   <- readInt16
                 return $ mkInterfaceMethodInfo (cpiName $ cpEntry cp clazz) (cpEntry cp nat)
        12 -> do name <- readInt16
                 desc <- readInt16
                 return $ CNameAndType (cpiStrValue $ cpEntry cp name) (parseDesc $ cpiStrValue $ cpEntry cp desc)
        _  -> error $ "Unknown tag " ++ show tag ++ " in CP"

------------------------------------------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------------------------------------------

-- TODO: catch errors?
parseDesc :: JString -> Either JType MethodSig
parseDesc s = parseDesc' (toString s)
  where parseDesc' ('(':s) = let (args, ')':rest) = span (/= ')') s
                                 params           = unfoldr oneType args
                                 retval           = case rest of
                                                      "V" -> Nothing
                                                      _   -> Just $ fst $ fromJust $ oneType rest
                             in Right $ MethodSig params retval

        parseDesc' s     = Left $ fst $ fromJust $ oneType s

        oneType :: String -> Maybe (JType, String)
        oneType ('[':s) = let (t, rest) = fromJust $ oneType s   in Just (TArray t, rest)
        oneType ('L':s) = let (name, ';':rest) = span (/= ';') s in Just (TInstance name, rest)
        oneType ('B':s) = Just (TByte, s)
        oneType ('C':s) = Just (TChar, s)
        oneType ('D':s) = Just (TDouble, s)
        oneType ('F':s) = Just (TFloat, s)
        oneType ('I':s) = Just (TInt, s)
        oneType ('J':s) = Just (TLong, s)
        oneType ('S':s) = Just (TShort, s)
        oneType ('Z':s) = Just (TBoolean, s)
        oneType (c:_)   = error $ "Failed to parse type " ++ show c
        oneType []      = Nothing


parseFieldType :: JString -> JType
parseFieldType = (\(Left t) -> t) . parseDesc

parseMethodSig :: JString -> MethodSig
parseMethodSig = (\(Right t) -> t) . parseDesc

cpEntry :: ConstantPool -> Int -> ConstantPoolInfo
cpEntry cp idx = (cpEntries cp) !! idx

mkFieldInfo :: JString -> ConstantPoolInfo -> ConstantPoolInfo
mkFieldInfo host (CNameAndType nam (Left typ)) = CField host nam typ
mkFieldInfo _ cpi = error $ "Expected NameAndType, but got " ++ (show cpi) ++ " instead"

mkMethodInfo :: JString -> ConstantPoolInfo -> ConstantPoolInfo
mkMethodInfo host (CNameAndType nam (Right sig)) = CMethod host nam sig
mkMethodInfo _ cpi = error $ "Expected NameAndType, but got " ++ (show cpi) ++ " instead"

mkInterfaceMethodInfo :: JString -> ConstantPoolInfo -> ConstantPoolInfo
mkInterfaceMethodInfo host (CNameAndType nam (Right sig)) = CInterfaceMethod host nam sig

readBytes :: Int -> Get [Word8]
readBytes n = unpack `liftM` (getBytes n)

readByte :: Get Word8
readByte = get

readWord16 :: Get Word16
readWord16 = get

readInt16 :: Get Int
readInt16 = fromIntegral `liftM` readWord16





