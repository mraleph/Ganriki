{-# LANGUAGE BangPatterns #-}

module Java.ClassParser.ConstantPool (
    ConstantPool
,   cpEntry
,   ConstantPoolInfo
,   parseConstantPool
,   MethodSig
,   msParams
,   msRetval
,   cpiName
,   cpiStrValue
,   cpiIntValue
,   cpiFloatValue
,   cpiLongValue
,   cpiDoubleValue
,   cpiFieldRef
,   cpiMethodRef
,   parseFieldType
,   parseMethodSig
,   FieldRef
,   MethodRef
,   frClass, frName, frType
,   mrClass, mrName, mrSig
,   getFieldRef
,   getMethodRef
,   getClassName
,   getJType
,   getConstant
) where

import Java.Types

import Data.Array

import Control.Monad (liftM, forM)
import Data.Word (Word8, Word16)
import Data.Int (Int64)
import Data.ByteString (unpack)
import qualified Data.ByteString.Lazy as L (ByteString)

import Data.List (unfoldr, intersperse, isPrefixOf)
import Data.Maybe (fromJust)

import Data.Binary
import Data.Binary.Get

-- introduce correction, after long/double must follow emtpy CNone
-- also prepend CNone
newtype ConstantPool = ConstantPool { cpEntries :: Array Int ConstantPoolInfo }

data MethodSig  = MethodSig  { msParams :: [JType], msRetval :: Maybe JType            }

instance Show MethodSig where
   show (MethodSig p r) = "(" ++ ps ++ ")" ++ (maybe "" ((" => " ++) . show) r)
                        where ps = concat $ intersperse ", " $ map show p

data FieldRef   = FieldRef  { frClass :: String, frName :: String, frType :: JType     }
data MethodRef  = MethodRef { mrClass :: String, mrName :: String, mrSig  :: MethodSig }

instance Show FieldRef where
  show (FieldRef h n t) = h ++ "." ++ n

instance Show MethodRef where
  show (MethodRef h n s) = h ++ "." ++ n ++ (show s)


data ConstantPoolInfo = CPClass           { cpiName        :: JString                              }
                      | CPField           { cpiFieldRef    :: FieldRef                             }
                      | CPMethod          { cpiMethodRef   :: MethodRef                            }
                      | CPInterfaceMethod { cpiMethodRef   :: MethodRef                            }
                      | CPString          { cpiStrValue    :: JString                              }
                      | CPInteger         { cpiIntValue    :: !JInt                                }
                      | CPFloat           { cpiFloatValue  :: !JFloat                              }
                      | CPLong            { cpiLongValue   :: !JLong                               }
                      | CPDouble          { cpiDoubleValue :: !JDouble                             }
                      | CPNameAndType     { cpiName :: JString, cpiDesc :: Either JType MethodSig  }
                      | CPUTF8            { cpiStrValue    :: !JString                             }
                      | CPNone
                      deriving (Show)

parseConstantPool :: Int -> Get ConstantPool
parseConstantPool n = do s <- getRemainingLazyByteString
                         let (cpSize, cp) = parse' n s
                         uncheckedSkip cpSize
                         return cp

parse' :: Int -> L.ByteString -> (Int64, ConstantPool)
parse' n s = (cpSize, cp)
   where getEntries pos = do
             !tag   <- readByte
             entry <- parseEntry cp pos tag
             let newpos    = if isDoubleEntry tag then pos + 2 else pos + 1             
             if newpos > n then return $ [(pos, entry)]
                           else ((pos, entry) : ) `liftM` getEntries newpos

         (cp, _, cpSize) = runGetState ((ConstantPool . array (1, n)) `liftM` getEntries 1) s 0

         isDoubleEntry 5 = True
         isDoubleEntry 6 = True
         isDoubleEntry _ = False


parseEntry :: ConstantPool -> Int -> Word8 -> Get ConstantPoolInfo
parseEntry cp i tag =
    case tag of
        1  -> do len <- readInt16  -- CONSTANT_Utf8_info
                 CPUTF8 `liftM` readJString len
        2  -> error "Tag 2 is unused"
        3  -> CPInteger `liftM` readJInt    -- CONSTANT_Integer_info
        4  -> CPFloat   `liftM` readJFloat  -- CONSTANT_Float_info
        5  -> CPLong    `liftM` readJLong   -- CONSTANT_Long_info
        6  -> CPDouble  `liftM` readJDouble -- CONSTANT_Double_info
        7  -> (CPClass . cpiStrValue . cpEntry cp)  `liftM` readInt16  -- CONSTANT_Class_info
        8  -> (CPString . cpiStrValue . cpEntry cp) `liftM` readInt16 -- CONSTANT_String_info
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
                 return $ CPNameAndType (cpiStrValue $ cpEntry cp name) (parseDesc $ cpiStrValue $ cpEntry cp desc)
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

-- TODO: maybe to Types?
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
cpEntry cp idx = (cpEntries cp) ! idx

mkFieldInfo :: JString -> ConstantPoolInfo -> ConstantPoolInfo
mkFieldInfo host (CPNameAndType nam (Left typ)) = CPField $ FieldRef (toString host) (toString nam) typ
mkFieldInfo _ cpi = error $ "Expected NameAndType, but got " ++ (show cpi) ++ " instead"

mkMethodInfo :: JString -> ConstantPoolInfo -> ConstantPoolInfo
mkMethodInfo host (CPNameAndType nam (Right sig)) = CPMethod $ MethodRef (toString host) (toString nam) sig
mkMethodInfo _ cpi = error $ "Expected NameAndType, but got " ++ (show cpi) ++ " instead"

mkInterfaceMethodInfo :: JString -> ConstantPoolInfo -> ConstantPoolInfo
mkInterfaceMethodInfo host (CPNameAndType nam (Right sig)) = CPInterfaceMethod $ MethodRef (toString host) (toString nam) sig

readByte :: Get Word8
readByte = get

readWord16 :: Get Word16
readWord16 = get

readInt16 :: Get Int
readInt16 = fromIntegral `liftM` readWord16



getFieldRef :: ConstantPool -> Int -> FieldRef
getFieldRef cp idx = case cpEntry cp idx of
                          CPField ref          -> ref
                          _                   -> error $ "There is no field reference at idx " ++ (show idx)


getMethodRef :: ConstantPool -> Int -> MethodRef
getMethodRef cp idx = case cpEntry cp idx of
                          CPMethod ref          -> ref
                          CPInterfaceMethod ref -> ref
                          _                    -> error $ "There is no method reference at idx " ++ (show idx)
  
        
getClassName :: ConstantPool -> Int -> String
getClassName cp idx = case cpEntry cp idx of
                          CPClass name -> toString name
                          _           -> error $ "There is no class reference at idx " ++ (show idx)                   

getJType :: ConstantPool -> Int -> JType
getJType cp idx = if "[" `isPrefixOf` name 
                      then fst $ fromJust $ oneType name 
                      else TInstance name
                  where name = getClassName cp idx

getConstant :: ConstantPool -> Int -> Constant
getConstant cp i = case cpEntry cp i of
                       CPString  s -> toConstant s
                       CPInteger i -> toConstant i
                       CPLong    l -> toConstant l
                       CPFloat   f -> toConstant f                               
                       CPDouble  d -> toConstant d
