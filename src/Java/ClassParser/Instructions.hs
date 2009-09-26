{-# LANGUAGE BangPatterns #-}

module Java.ClassParser.Instructions (
  parseCode
, Bytecode, bcCode
, VMOp(..)
, VMOperandType (..)
, BranchCondition(..)
, ArrayType, ArrayBaseType, abType, atBaseType, atDim) where

import Util.QObject

import Data.Maybe (fromJust)
import Control.Monad (ap)
import Control.Monad.Loops (whileM)
import Control.Monad.Reader
import Control.Arrow (second)

import Data.Array

import Data.Word (Word8, Word16, Word32)

import Data.Int (Int16, Int32, Int64)

import Data.Binary (get)
import Data.Binary.Strict.Get

import qualified Data.ByteString as StrictB

import qualified Java.Types as T (toString, Constant(..), JType(..))
import Java.ClassParser.ConstantPool (ConstantPool, FieldRef, MethodRef, getFieldRef, getMethodRef, getClassName, getJType, cpEntry, ConstantPoolInfo(..), getConstant)

data VMOperandType = OpRef                   
                   | OpBoolean
                   | OpByte
                   | OpChar
                   | OpShort         
                   | OpInt
                   | OpLong
                   | OpFloat
                   | OpDouble                  
                   deriving (Show)

data BranchCondition = ZEq
                     | ZNe
                     | ZLt
                     | ZGe
                     | ZGt
                     | ZLe
                     | Eq
                     | Ne
                     | Lt
                     | Ge
                     | Gt
                     | Le
                     | RefEq
                     | RefNe
                     | Null
                     | NotNull
                     deriving (Show)

newtype ArrayBaseType = ArrayBaseType { abType :: Either VMOperandType String } deriving (Show)



data ArrayType = ArrayType { atBaseType :: ArrayBaseType, atDim :: Int } deriving (Show)

instance QObject ArrayBaseType where
    render t = 
        case abType t of 
            Left t  -> render t
            Right s -> s

instance QObject ArrayType where
    render (ArrayType base dims) =  (render base) ++ (concat $ replicate dims "[]")

instance QObject VMOperandType where
    render t =
        case t of
            OpRef     -> "ref"
            OpBoolean -> "boolean"
            OpByte    -> "byte"
            OpChar    -> "char"
            OpShort   -> "short"
            OpInt     -> "int"
            OpLong    -> "long"
            OpFloat   -> "float"
            OpDouble  -> "double"

data VMOp = Nop
          | Push   { const :: T.Constant }
          | Load   { vmopType :: VMOperandType, local :: Int } 
          | Store  { vmopType :: VMOperandType, local :: Int }
          | ALoad  { vmopType :: VMOperandType }
          | AStore { vmopType :: VMOperandType }
          | Pop
          | Pop2
          | Dup
          | DupX1
          | DupX2
          | Dup2
          | Dup2X1
          | Dup2X2
          | Swap
          | Add  { vmopType :: VMOperandType }
          | Sub  { vmopType :: VMOperandType }
          | Mul  { vmopType :: VMOperandType }
          | Div  { vmopType :: VMOperandType }
          | Rem  { vmopType :: VMOperandType }
          | Neg  { vmopType :: VMOperandType }
          | Shl  { vmopType :: VMOperandType }
          | Shr  { vmopType :: VMOperandType }
          | UShr { vmopType :: VMOperandType }
          | And  { vmopType :: VMOperandType }
          | Or   { vmopType :: VMOperandType }
          | XOr  { vmopType :: VMOperandType }
          | IInc { local :: Int, inc :: Int }
          | Coerce { fromtype :: VMOperandType, totype :: VMOperandType }
          | Cmp  { vmopType :: VMOperandType }
          | GotoIf { branchCondition :: BranchCondition, branchTarget :: Int }
          | Goto   { branchTarget :: Int }
          | JSR    { branchTarget :: Int }
          | Ret    { local :: Int }
          | Return { rettype :: Maybe VMOperandType}
          | GetStatic { fieldref :: FieldRef }
          | PutStatic { fieldref :: FieldRef }
          | GetField  { fieldref :: FieldRef }
          | PutField  { fieldref :: FieldRef }
          | InvokeSpecial   { methodref :: MethodRef }
          | InvokeStatic    { methodref :: MethodRef }
          | InvokeVirtual   { methodref :: MethodRef }
          | InvokeInterface { methodref :: MethodRef }
          | New  { classref :: String }
          | ANew { basetype :: ArrayBaseType }
          | ALength
          | Throw
          | CheckCast   { classref :: String } -- TODO [!!] arrays can come here to
          | InstanceOf  { classref :: String }
          | MonitorEnter
          | MonitorExit
          | MultiNew { multiarraytype :: ArrayType, dimensions :: Int }          
          | LookupSwitch { lookuptable :: [(Int32, Int)], def :: Int }
          | TableSwitch { tablelow :: Int32, tablehigh :: Int32, tableoffs :: [Int], def :: Int }
          deriving (Show)


data Bytecode = Bytecode { bcCode :: [VMOp], bcLabels :: [Int] }

instance Show Bytecode where
    show b = unlines $ zipWith f [0..(n-1)] code 
        where code  = bcCode b
              n     = length $ bcCode b
              f i l = ("\t\t" ++ (show i) ++ ": ") ++ (show l)

data St  = St 
           { stConstantPool :: ConstantPool
           , stLabel        :: Int -> Int
           , stOffset       :: Int
           }

type CPGet a = ReaderT St Get a

data VMInstruction = VMInstruction { vmiName :: String, vmiOpcode :: Int, vmiParse :: CPGet VMOp }

parseCode :: ConstantPool -> StrictB.ByteString -> (Bytecode, Int -> Int)
parseCode cp code = (Bytecode ops [], label)
    where
        (o2i, ops) = fromRight $ fst $ runGet (parseCode' cp label 0) code
        
        label offs = case lookup offs o2i of
                         Nothing  -> error $ "Offset " ++ (show offs) ++ " not found in offset table: " ++ (show o2i)
                         Just idx -> idx

        fromRight x = case x of
                          Left s  -> error s
                          Right v -> v

{-    
    where 
        -- TODO: think whether possible to elegantly generate labels during parsing?
        offs2index :: [(Int, VMOp)] -> Int -> [(Int, Int)]
        labels ((offs, _):ops) i =  (offs, i):(offs2index ops (i + 1))

        mkLabels :: [(Int, Int)] -> [(Int, VMOp)] -> ([VMOp], [Int]) -> ([VMOp], [Int])
        mkLabels l2i (op:ops) (ops', labels')  = case op of
                                                 GotoIf _ _ -> 
                                                 Goto _     ->
                                                 JSR _      ->
                                                 x          -> mkLabels 
        mkLabels l2i []       (ops', labels')  = (reverse ops', reverse labels')
-}
        



parseCode' :: ConstantPool -> (Int -> Int) -> Int -> Get ([(Int, Int)], [VMOp])
parseCode' cp label idx = do
    empty <- isEmpty
    if empty 
        then return ([], [])
        else do 
            offs   <- bytesRead;
            opcode <- fromIntegral `liftM` getWord8;
            let instr = instructionSet ! opcode;
            op         <- runReaderT (vmiParse instr) (St cp label offs)
            (o2i, ops) <- parseCode' cp label (idx+1)
            return $ ((offs, idx):o2i, op:ops)


instructionSet :: Array Int VMInstruction
instructionSet = array (0, 255) $ map (\x -> (vmiOpcode x, x)) [    
    VMInstruction "nop"           0     $ return Nop
    ,   VMInstruction "aconst_null"   1 $ return $ Push $ T.CNull
    ,   VMInstruction "iconst_m1"     2 $ return $ Push $ T.CInt (-1)
    ,   VMInstruction "iconst_0"      3 $ return $ Push $ T.CInt 0
    ,   VMInstruction "iconst_1"      4 $ return $ Push $ T.CInt 1
    ,   VMInstruction "iconst_2"      5 $ return $ Push $ T.CInt 2
    ,   VMInstruction "iconst_3"      6 $ return $ Push $ T.CInt 3
    ,   VMInstruction "iconst_4"      7 $ return $ Push $ T.CInt 4
    ,   VMInstruction "iconst_5"      8 $ return $ Push $ T.CInt 5
    ,   VMInstruction "lconst_0"      9 $ return $ Push $ T.CLong 0
    ,   VMInstruction "lconst_1"     10 $ return $ Push $ T.CLong 1
    ,   VMInstruction "fconst_0"     11 $ return $ Push $ T.CFloat 0.0
    ,   VMInstruction "fconst_1"     12 $ return $ Push $ T.CFloat 1.0
    ,   VMInstruction "fconst_2"     13 $ return $ Push $ T.CFloat 2.0
    ,   VMInstruction "dconst_0"     14 $ return $ Push $ T.CDouble 0.0
    ,   VMInstruction "dconst_1"     15 $ return $ Push $ T.CDouble 1.0

    ,   VMInstruction "bipush"       16 $ (Push . T.CByte  ) `liftM` readByte
    ,   VMInstruction "sipush"       17 $ (Push . T.CShort ) `liftM` readShort

    ,   VMInstruction "ldc"          18 $ Push `liftM` (liftM2 getConstant getCP readByte)
    ,   VMInstruction "ldc_w"        19 $ Push `liftM` (liftM2 getConstant getCP readShort)
    ,   VMInstruction "ldc2_w"       20 $ Push `liftM` (liftM2 getConstant getCP readShort)

    ,   VMInstruction "iload"        21 $ (Load OpInt)    `liftM` readByte
    ,   VMInstruction "lload"        22 $ (Load OpLong)   `liftM` readByte
    ,   VMInstruction "fload"        23 $ (Load OpFloat)  `liftM` readByte
    ,   VMInstruction "dload"        24 $ (Load OpDouble) `liftM` readByte
    ,   VMInstruction "aload"        25 $ (Load OpRef)    `liftM` readByte

    ,   VMInstruction "iload_0"      26 $ return $ Load OpInt 0
    ,   VMInstruction "iload_1"      27 $ return $ Load OpInt 1
    ,   VMInstruction "iload_2"      28 $ return $ Load OpInt 2
    ,   VMInstruction "iload_3"      29 $ return $ Load OpInt 3
    ,   VMInstruction "lload_0"      30 $ return $ Load OpLong 0
    ,   VMInstruction "lload_1"      31 $ return $ Load OpLong 1
    ,   VMInstruction "lload_2"      32 $ return $ Load OpLong 2
    ,   VMInstruction "lload_3"      33 $ return $ Load OpLong 3
    ,   VMInstruction "fload_0"      34 $ return $ Load OpFloat 0
    ,   VMInstruction "fload_1"      35 $ return $ Load OpFloat 1
    ,   VMInstruction "fload_2"      36 $ return $ Load OpFloat 2
    ,   VMInstruction "fload_3"      37 $ return $ Load OpFloat 3
    ,   VMInstruction "dload_0"      38 $ return $ Load OpDouble 0
    ,   VMInstruction "dload_1"      39 $ return $ Load OpDouble 1
    ,   VMInstruction "dload_2"      40 $ return $ Load OpDouble 2
    ,   VMInstruction "dload_3"      41 $ return $ Load OpDouble 3
    ,   VMInstruction "aload_0"      42 $ return $ Load OpRef 0 
    ,   VMInstruction "aload_1"      43 $ return $ Load OpRef 1
    ,   VMInstruction "aload_2"      44 $ return $ Load OpRef 2
    ,   VMInstruction "aload_3"      45 $ return $ Load OpRef 3

    ,   VMInstruction "iaload"       46 $ return $ ALoad OpInt
    ,   VMInstruction "laload"       47 $ return $ ALoad OpLong
    ,   VMInstruction "faload"       48 $ return $ ALoad OpFloat
    ,   VMInstruction "daload"       49 $ return $ ALoad OpDouble
    ,   VMInstruction "aaload"       50 $ return $ ALoad OpRef
    ,   VMInstruction "baload"       51 $ return $ ALoad OpByte
    ,   VMInstruction "caload"       52 $ return $ ALoad OpChar
    ,   VMInstruction "saload"       53 $ return $ ALoad OpShort

    ,   VMInstruction "istore"       54 $ Store OpInt    `liftM` readByte
    ,   VMInstruction "lstore"       55 $ Store OpLong   `liftM` readByte
    ,   VMInstruction "fstore"       56 $ Store OpFloat  `liftM` readByte
    ,   VMInstruction "dstore"       57 $ Store OpDouble `liftM` readByte
    ,   VMInstruction "astore"       58 $ Store OpRef    `liftM` readByte

    ,   VMInstruction "istore_0"     59 $ return $ Store OpInt 0
    ,   VMInstruction "istore_1"     60 $ return $ Store OpInt 1
    ,   VMInstruction "istore_2"     61 $ return $ Store OpInt 2
    ,   VMInstruction "istore_3"     62 $ return $ Store OpInt 3
    ,   VMInstruction "lstore_0"     63 $ return $ Store OpLong 0
    ,   VMInstruction "lstore_1"     64 $ return $ Store OpLong 1
    ,   VMInstruction "lstore_2"     65 $ return $ Store OpLong 2
    ,   VMInstruction "lstore_3"     66 $ return $ Store OpLong 3
    ,   VMInstruction "fstore_0"     67 $ return $ Store OpFloat 0
    ,   VMInstruction "fstore_1"     68 $ return $ Store OpFloat 1
    ,   VMInstruction "fstore_2"     69 $ return $ Store OpFloat 2
    ,   VMInstruction "fstore_3"     70 $ return $ Store OpFloat 3
    ,   VMInstruction "dstore_0"     71 $ return $ Store OpDouble 0
    ,   VMInstruction "dstore_1"     72 $ return $ Store OpDouble 1
    ,   VMInstruction "dstore_2"     73 $ return $ Store OpDouble 2
    ,   VMInstruction "dstore_3"     74 $ return $ Store OpDouble 3
    ,   VMInstruction "astore_0"     75 $ return $ Store OpRef 0
    ,   VMInstruction "astore_1"     76 $ return $ Store OpRef 1
    ,   VMInstruction "astore_2"     77 $ return $ Store OpRef 2
    ,   VMInstruction "astore_3"     78 $ return $ Store OpRef 3

    ,   VMInstruction "iastore"      79 $ return $ AStore OpInt
    ,   VMInstruction "lastore"      80 $ return $ AStore OpLong
    ,   VMInstruction "fastore"      81 $ return $ AStore OpFloat
    ,   VMInstruction "dastore"      82 $ return $ AStore OpDouble
    ,   VMInstruction "aastore"      83 $ return $ AStore OpRef
    ,   VMInstruction "bastore"      84 $ return $ AStore OpByte
    ,   VMInstruction "castore"      85 $ return $ AStore OpChar
    ,   VMInstruction "sastore"      86 $ return $ AStore OpShort

    ,   VMInstruction "pop"          87 $ return Pop
    ,   VMInstruction "pop2"         88 $ return Pop2
    ,   VMInstruction "dup"          89 $ return Dup
    ,   VMInstruction "dup_x1"       90 $ return DupX1
    ,   VMInstruction "dup_x2"       91 $ return DupX2
    ,   VMInstruction "dup2"         92 $ return Dup2
    ,   VMInstruction "dup2_x1"      93 $ return Dup2X1
    ,   VMInstruction "dup2_x2"      94 $ return Dup2X2
    ,   VMInstruction "swap"         95 $ return Swap

    ,   VMInstruction "iadd"         96 $ return $ Add OpInt
    ,   VMInstruction "ladd"         97 $ return $ Add OpLong
    ,   VMInstruction "fadd"         98 $ return $ Add OpFloat
    ,   VMInstruction "dadd"         99 $ return $ Add OpDouble

    ,   VMInstruction "isub"        100 $ return $ Sub OpInt
    ,   VMInstruction "lsub"        101 $ return $ Sub OpLong
    ,   VMInstruction "fsub"        102 $ return $ Sub OpFloat
    ,   VMInstruction "dsub"        103 $ return $ Sub OpDouble

    ,   VMInstruction "imul"        104 $ return $ Mul OpInt
    ,   VMInstruction "lmul"        105 $ return $ Mul OpLong
    ,   VMInstruction "fmul"        106 $ return $ Mul OpFloat
    ,   VMInstruction "dmul"        107 $ return $ Mul OpDouble

    ,   VMInstruction "idiv"        108 $ return $ Div OpInt
    ,   VMInstruction "ldiv"        109 $ return $ Div OpLong
    ,   VMInstruction "fdiv"        110 $ return $ Div OpFloat
    ,   VMInstruction "ddiv"        111 $ return $ Div OpDouble


    ,   VMInstruction "irem"        112 $ return $ Rem OpInt
    ,   VMInstruction "lrem"        113 $ return $ Rem OpLong
    ,   VMInstruction "frem"        114 $ return $ Rem OpFloat
    ,   VMInstruction "drem"        115 $ return $ Rem OpDouble

    ,   VMInstruction "ineg"        116 $ return $ Neg OpInt
    ,   VMInstruction "lneg"        117 $ return $ Neg OpLong
    ,   VMInstruction "fneg"        118 $ return $ Neg OpFloat
    ,   VMInstruction "dneg"        119 $ return $ Neg OpDouble

    ,   VMInstruction "ishl"        120 $ return $ Shl OpInt
    ,   VMInstruction "lshl"        121 $ return $ Shl OpLong
    ,   VMInstruction "ishr"        122 $ return $ Shr OpInt
    ,   VMInstruction "lshr"        123 $ return $ Shr OpLong
    ,   VMInstruction "iushr"       124 $ return $ UShr OpInt
    ,   VMInstruction "lushr"       125 $ return $ UShr OpLong
    ,   VMInstruction "iand"        126 $ return $ And OpInt
    ,   VMInstruction "land"        127 $ return $ And OpLong
    ,   VMInstruction "ior"         128 $ return $ Or OpInt
    ,   VMInstruction "lor"         129 $ return $ Or OpLong
    ,   VMInstruction "ixor"        130 $ return $ XOr OpInt
    ,   VMInstruction "lxor"        131 $ return $ XOr OpLong

    ,   VMInstruction "iinc"        132 $ liftM2 IInc readByte readByte

    ,   VMInstruction "i2l"         133 $ return $ Coerce OpInt OpLong  
    ,   VMInstruction "i2f"         134 $ return $ Coerce OpInt OpFloat  
    ,   VMInstruction "i2d"         135 $ return $ Coerce OpInt OpDouble 
    ,   VMInstruction "l2i"         136 $ return $ Coerce OpLong OpInt
    ,   VMInstruction "l2f"         137 $ return $ Coerce OpLong OpFloat
    ,   VMInstruction "l2d"         138 $ return $ Coerce OpLong OpDouble
    ,   VMInstruction "f2i"         139 $ return $ Coerce OpFloat OpInt
    ,   VMInstruction "f2l"         140 $ return $ Coerce OpFloat OpLong
    ,   VMInstruction "f2d"         141 $ return $ Coerce OpFloat OpDouble
    ,   VMInstruction "d2i"         142 $ return $ Coerce OpDouble OpInt
    ,   VMInstruction "d2l"         143 $ return $ Coerce OpDouble OpLong
    ,   VMInstruction "d2f"         144 $ return $ Coerce OpDouble OpFloat
    ,   VMInstruction "i2b"         145 $ return $ Coerce OpInt OpByte
    ,   VMInstruction "i2c"         146 $ return $ Coerce OpInt OpChar
    ,   VMInstruction "i2s"         147 $ return $ Coerce OpInt OpShort


    ,   VMInstruction "lcmp"        148 $ return $ Cmp OpLong
    ,   VMInstruction "fcmpl"       149 $ return $ Cmp OpFloat   -- todo: g/l difference
    ,   VMInstruction "fcmpg"       150 $ return $ Cmp OpFloat   -- todo: g/l difference
    ,   VMInstruction "dcmpl"       151 $ return $ Cmp OpDouble  -- todo: g/l difference
    ,   VMInstruction "dcmpg"       152 $ return $ Cmp OpDouble  -- todo: g/l difference

    ,   VMInstruction "ifeq"        153 $ GotoIf ZEq   `liftM` readShortBranchOffset
    ,   VMInstruction "ifne"        154 $ GotoIf ZNe   `liftM` readShortBranchOffset
    ,   VMInstruction "iflt"        155 $ GotoIf ZLt   `liftM` readShortBranchOffset
    ,   VMInstruction "ifge"        156 $ GotoIf ZGe   `liftM` readShortBranchOffset
    ,   VMInstruction "ifgt"        157 $ GotoIf ZGt   `liftM` readShortBranchOffset
    ,   VMInstruction "ifle"        158 $ GotoIf ZLe   `liftM` readShortBranchOffset
    ,   VMInstruction "if_icmpeq"   159 $ GotoIf Eq    `liftM` readShortBranchOffset
    ,   VMInstruction "if_icmpne"   160 $ GotoIf Ne    `liftM` readShortBranchOffset
    ,   VMInstruction "if_icmplt"   161 $ GotoIf Lt    `liftM` readShortBranchOffset
    ,   VMInstruction "if_icmpge"   162 $ GotoIf Ge    `liftM` readShortBranchOffset
    ,   VMInstruction "if_icmpgt"   163 $ GotoIf Gt    `liftM` readShortBranchOffset
    ,   VMInstruction "if_icmple"   164 $ GotoIf Le    `liftM` readShortBranchOffset
    ,   VMInstruction "if_acmpeq"   165 $ GotoIf RefEq `liftM` readShortBranchOffset
    ,   VMInstruction "if_acmpne"   166 $ GotoIf RefNe `liftM` readShortBranchOffset
    ,   VMInstruction "goto"        167 $ Goto         `liftM` readShortBranchOffset
    ,   VMInstruction "jsr"         168 $ JSR          `liftM` readShortBranchOffset
    ,   VMInstruction "ret"         169 $ Ret          `liftM` readByte

    ,   VMInstruction "tableswitch"  170 $ tableswitch
    ,   VMInstruction "lookupswitch" 171 $ lookupswitch

    ,   VMInstruction "ireturn"     172 $ return $ Return $ Just OpInt
    ,   VMInstruction "lreturn"     173 $ return $ Return $ Just OpLong
    ,   VMInstruction "freturn"     174 $ return $ Return $ Just OpFloat
    ,   VMInstruction "dreturn"     175 $ return $ Return $ Just OpDouble
    ,   VMInstruction "areturn"     176 $ return $ Return $ Just OpRef
    ,   VMInstruction "return"      177 $ return $ Return $ Nothing

    ,   VMInstruction "getstatic"   178 $ GetStatic `liftM` readFieldRef
    ,   VMInstruction "putstatic"   179 $ PutStatic `liftM` readFieldRef

    ,   VMInstruction "getfield"    180 $ GetField `liftM` readFieldRef
    ,   VMInstruction "putfield"    181 $ PutField `liftM` readFieldRef

    ,   VMInstruction "invokevirtual"   182 $ InvokeVirtual `liftM` readMethodRef
    ,   VMInstruction "invokespecial"   183 $ InvokeSpecial `liftM` readMethodRef
    ,   VMInstruction "invokestatic"    184 $ InvokeStatic  `liftM` readMethodRef
    ,   VMInstruction "invokeinterface" 185 $ do { ref <- readShort; lift $ skip 2;  cp <- getCP; return $ InvokeInterface $ getMethodRef cp ref }

    ,   VMInstruction "new"           187 $ New `liftM` readClassName

    ,   VMInstruction "newarray"      188 $ (ANew . ArrayBaseType . Left  . fromAType)    `liftM` readByte
    ,   VMInstruction "anewarray"     189 $ (ANew . ArrayBaseType . Right) `liftM` readClassName

    ,   VMInstruction "arraylength"   190 $ return ALength
    ,   VMInstruction "athrow"        191 $ return Throw

    ,   VMInstruction "checkcast"   192 $ CheckCast  `liftM` readClassName
    ,   VMInstruction "instanceof"  193 $ InstanceOf `liftM` readClassName

    ,   VMInstruction "monitorenter"  194 $ return MonitorEnter 
    ,   VMInstruction "monitorexit"   195 $ return MonitorExit

    ,   VMInstruction "wide"        196 $ parseWide

    ,   VMInstruction "multinewarray" 197 $ liftM2 MultiNew readArrayType (readByte)

    ,   VMInstruction "ifnull"      198 $ (GotoIf Null)    `liftM` readShortBranchOffset
    ,   VMInstruction "ifnonnull"   199 $ (GotoIf NotNull) `liftM` readShortBranchOffset
    ,   VMInstruction "goto_w"      200 $ Goto `liftM` readIntBranchOffset
    ,   VMInstruction "jsr_w"       201 $ JSR  `liftM` readIntBranchOffset] 
    where        
        fromAType   :: Int -> VMOperandType
        fromAType  4 = OpBoolean
        fromAType  5 = OpChar 
        fromAType  6 = OpFloat
        fromAType  7 = OpDouble
        fromAType  8 = OpByte
        fromAType  9 = OpShort
        fromAType 10 = OpInt
        fromAType 11 = OpLong
        fromAType  i = error $ "invalid array type " ++ show i

        getArrayType :: ConstantPool -> Int -> ArrayType
        getArrayType cp i = uncurry ArrayType $ jtype2arraytype $ getJType cp i

        getCP     = stConstantPool `liftM` ask        
        getOpOffs = stOffset       `liftM` ask
        label     = stLabel        `liftM` ask

        readIntBranchOffset :: CPGet Int
        readIntBranchOffset = do
            label `ap` liftM2 (+) readSignedInt getOpOffs


        readShortBranchOffset :: CPGet Int
        readShortBranchOffset = do
            label `ap` liftM2 (+) readSignedShort getOpOffs
            


        jtype2arraytype :: T.JType -> (ArrayBaseType, Int)
        jtype2arraytype (T.TArray b) = case b of 
                                           T.TArray _    -> second (+1) $ jtype2arraytype b
                                           T.TInstance i -> (ArrayBaseType $ Right i, 0)
                                           T.TByte       -> (ArrayBaseType $ Left OpByte, 0)
                                           T.TChar       -> (ArrayBaseType $ Left OpChar, 0)
                                           T.TDouble     -> (ArrayBaseType $ Left OpDouble, 0)
                                           T.TFloat      -> (ArrayBaseType $ Left OpFloat, 0)
                                           T.TInt        -> (ArrayBaseType $ Left OpInt, 0)
                                           T.TLong       -> (ArrayBaseType $ Left OpLong, 0)
                                           T.TShort      -> (ArrayBaseType $ Left OpShort, 0)
                                           T.TBoolean    -> (ArrayBaseType $ Left OpBoolean, 0)


        readByte :: Integral a => CPGet a
        readByte = lift (fromIntegral `liftM` getWord8) 

        readShort :: Integral a => CPGet a
        readShort = lift (fromIntegral `liftM` getWord16be)
        
        readInt :: Integral a => CPGet a
        readInt = lift (fromIntegral `liftM` getWord32be)

        readSignedShort :: CPGet Int
        readSignedShort = fromIntegral `liftM` lift (u16_to_s16 `liftM` getWord16be)

        readSignedInt   :: CPGet Int
        readSignedInt   = fromIntegral `liftM` lift (u32_to_s32 `liftM` getWord32be)

        u16_to_s16 :: Word16 -> Int16
        u16_to_s16 = fromIntegral

        u32_to_s32 :: Word32 -> Int32
        u32_to_s32 = fromIntegral        

        readFieldRef  = liftM2 getFieldRef  getCP readShort
        readMethodRef = liftM2 getMethodRef getCP readShort
        readClassName = liftM2 getClassName getCP readShort 
        readArrayType = liftM2 getArrayType getCP readShort 

        pad :: Int -> CPGet ()
        pad n = do
            offs <- lift bytesRead
            let m = offs `mod` n
            lift $ skip $ if m == 0 then 0 else fromIntegral (n - m)

        lookupswitch = do
            pad 4
            def     <- readIntBranchOffset
            npairs  <- readInt
            pairs   <- replicateM npairs (liftM2 (,) readInt readIntBranchOffset)
            return $ LookupSwitch pairs def

        tableswitch = do
            pad 4
            def  <- readIntBranchOffset
            low  <- readInt
            high <- readInt
            offs <- replicateM (fromIntegral $ high - low + 1) readIntBranchOffset
            return $ TableSwitch low high offs def

        parseWide = do
            opcode <- readByte
            case opcode of 
                -- iload, lload, fload, dload, aload    
                21 -> Load OpInt    `liftM` readShort
                22 -> Load OpLong   `liftM` readShort
                23 -> Load OpFloat  `liftM` readShort
                24 -> Load OpDouble `liftM` readShort
                25 -> Load OpRef    `liftM` readShort
                -- istore, lstore, fstore, dstore, astore
                54 -> Store OpInt    `liftM` readShort
                55 -> Store OpLong   `liftM` readShort
                56 -> Store OpFloat  `liftM` readShort
                57 -> Store OpDouble `liftM` readShort
                58 -> Store OpRef    `liftM` readShort
                -- ret
                169 -> Ret `liftM` readShort
                -- iinc
                132 -> liftM2 IInc readShort readShort
                _   -> error $ "invalid wide operation " ++ show opcode
                                                     
                                                     



        

    

