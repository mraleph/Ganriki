module Java.ClassParser.Instructions (VMOp, parseCode) where

import Control.Monad.Reader

import Data.Array

import Data.Int (Int32, Int64)

import Data.Binary
import Data.Binary.Get

import qualified Java.Types as T (toString, Constant(..), JType(..))
import Java.ClassParser.ConstantPool (ConstantPool, FieldRef, MethodRef, getFieldRef, getMethodRef, getClassName, getJType, cpEntry, ConstantPoolInfo(..), getConstant)

data VMOperandType = TRef                   
                   | TBoolean
                   | TByte
                   | TChar
                   | TShort         
                   | TInt
                   | TLong
                   | TFloat
                   | TDouble                  
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

data ArrayType = ArrayType { atBaseType :: Either VMOperandType String, atDim :: Int } deriving (Show)

--
-- TODO [!!] it looks like branchTargets are byte offsets, not indexes, so invent a way to convert them
-- Maybe use some self-ref trick again?
--
data VMOp = Nop
          | Push   { const :: T.Constant }
          | Load   { optype :: VMOperandType, local :: Int } 
          | Store  { optype :: VMOperandType, local :: Int }
          | ALoad  { optype :: VMOperandType }
          | AStore { optype :: VMOperandType }
          | Pop
          | Pop2
          | Dup
          | DupX1
          | DupX2
          | Dup2
          | Dup2X1
          | Dup2X2
          | Swap
          | Add  { optype :: VMOperandType }
          | Sub  { optype :: VMOperandType }
          | Mul  { optype :: VMOperandType }
          | Div  { optype :: VMOperandType }
          | Rem  { optype :: VMOperandType }
          | Neg  { optype :: VMOperandType }
          | Shl  { optype :: VMOperandType }
          | Shr  { optype :: VMOperandType }
          | UShr { optype :: VMOperandType }
          | And  { optype :: VMOperandType }
          | Or   { optype :: VMOperandType }
          | XOr  { optype :: VMOperandType }
          | IInc { local :: Int, inc :: Int }
          | Coerce { fromtype :: VMOperandType, totype :: VMOperandType }
          | Cmp  { optype :: VMOperandType }
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
          | ANew { basetype :: Either VMOperandType String }
          | ALength
          | Throw
          | CheckCast   { classref :: String } -- TODO [!!] arrays can come here to
          | InstanceOf  { classref :: String }
          | MonitorEnter
          | MonitorExit
          | MultiNew { multiarraytype :: ArrayType, dimensions :: Int }          
          | LookupSwitch { lookuptable :: [(Int32, Int32)], def :: Int32 }
          | TableSwitch { tablelow :: Int32, tablehigh :: Int32, tableoffs :: [Int32] }
          deriving (Show)




type CPGet a = ReaderT ConstantPool Get a

data VMInstruction = VMInstruction { vmiName :: String, vmiOpcode :: Int, vmiParse :: CPGet VMOp }
      
parseCode :: ConstantPool -> Get [VMOp]
parseCode = parseCode' [] 

-- TODO think about this shit function for a while
parseCode' :: [VMOp] -> ConstantPool -> Get [VMOp]
parseCode' ops cp = do
    empty <- isEmpty
    if empty 
        then return $ reverse $ ops
        else do opcode <- fromIntegral `liftM` (get :: Get Word8) -- TODO: create utilities module!
                let instr = instructionSet ! opcode    
                op <- runReaderT ( vmiParse instr ) cp
                parseCode' (op:ops) cp
    

instructionSet :: Array Int VMInstruction
instructionSet = array (0, 255) $ map (\x -> (vmiOpcode x, x)) [    
    VMInstruction "nop"           0 $ return Nop
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

    ,   VMInstruction "ldc"          18 $ Push `liftM` (liftM2 getConstant ask readByte)
    ,   VMInstruction "ldc_w"        19 $ Push `liftM` (liftM2 getConstant ask readShort)
    ,   VMInstruction "ldc2_w"       20 $ Push `liftM` (liftM2 getConstant ask readShort)

    ,   VMInstruction "iload"        21 $ (Load TInt)    `liftM` readByte
    ,   VMInstruction "lload"        22 $ (Load TLong)   `liftM` readByte
    ,   VMInstruction "fload"        23 $ (Load TFloat)  `liftM` readByte
    ,   VMInstruction "dload"        24 $ (Load TDouble) `liftM` readByte
    ,   VMInstruction "aload"        25 $ (Load TRef)    `liftM` readByte

    ,   VMInstruction "iload_0"      26 $ return $ Load TInt 0
    ,   VMInstruction "iload_1"      27 $ return $ Load TInt 1
    ,   VMInstruction "iload_2"      28 $ return $ Load TInt 2
    ,   VMInstruction "iload_3"      29 $ return $ Load TInt 3
    ,   VMInstruction "lload_0"      30 $ return $ Load TLong 0
    ,   VMInstruction "lload_1"      31 $ return $ Load TLong 1
    ,   VMInstruction "lload_2"      32 $ return $ Load TLong 2
    ,   VMInstruction "lload_3"      33 $ return $ Load TLong 3
    ,   VMInstruction "fload_0"      34 $ return $ Load TFloat 0
    ,   VMInstruction "fload_1"      35 $ return $ Load TFloat 1
    ,   VMInstruction "fload_2"      36 $ return $ Load TFloat 2
    ,   VMInstruction "fload_3"      37 $ return $ Load TFloat 3
    ,   VMInstruction "dload_0"      38 $ return $ Load TDouble 0
    ,   VMInstruction "dload_1"      39 $ return $ Load TDouble 1
    ,   VMInstruction "dload_2"      40 $ return $ Load TDouble 2
    ,   VMInstruction "dload_3"      41 $ return $ Load TDouble 3
    ,   VMInstruction "aload_0"      42 $ return $ Load TRef 0 
    ,   VMInstruction "aload_1"      43 $ return $ Load TRef 1
    ,   VMInstruction "aload_2"      44 $ return $ Load TRef 2
    ,   VMInstruction "aload_3"      45 $ return $ Load TRef 3

    ,   VMInstruction "iaload"       46 $ return $ ALoad TInt
    ,   VMInstruction "laload"       47 $ return $ ALoad TLong
    ,   VMInstruction "faload"       48 $ return $ ALoad TFloat
    ,   VMInstruction "daload"       49 $ return $ ALoad TDouble
    ,   VMInstruction "aaload"       50 $ return $ ALoad TRef
    ,   VMInstruction "baload"       51 $ return $ ALoad TByte
    ,   VMInstruction "caload"       52 $ return $ ALoad TChar
    ,   VMInstruction "saload"       53 $ return $ ALoad TShort

    ,   VMInstruction "istore"       54 $ Store TInt    `liftM` readByte
    ,   VMInstruction "lstore"       55 $ Store TLong   `liftM` readByte
    ,   VMInstruction "fstore"       56 $ Store TFloat  `liftM` readByte
    ,   VMInstruction "dstore"       57 $ Store TDouble `liftM` readByte
    ,   VMInstruction "astore"       58 $ Store TRef    `liftM` readByte

    ,   VMInstruction "istore_0"     59 $ return $ Store TInt 0
    ,   VMInstruction "istore_1"     60 $ return $ Store TInt 1
    ,   VMInstruction "istore_2"     61 $ return $ Store TInt 2
    ,   VMInstruction "istore_3"     62 $ return $ Store TInt 3
    ,   VMInstruction "lstore_0"     63 $ return $ Store TLong 0
    ,   VMInstruction "lstore_1"     64 $ return $ Store TLong 1
    ,   VMInstruction "lstore_2"     65 $ return $ Store TLong 2
    ,   VMInstruction "lstore_3"     66 $ return $ Store TLong 3
    ,   VMInstruction "fstore_0"     67 $ return $ Store TFloat 0
    ,   VMInstruction "fstore_1"     68 $ return $ Store TFloat 1
    ,   VMInstruction "fstore_2"     69 $ return $ Store TFloat 2
    ,   VMInstruction "fstore_3"     70 $ return $ Store TFloat 3
    ,   VMInstruction "dstore_0"     71 $ return $ Store TDouble 0
    ,   VMInstruction "dstore_1"     72 $ return $ Store TDouble 1
    ,   VMInstruction "dstore_2"     73 $ return $ Store TDouble 2
    ,   VMInstruction "dstore_3"     74 $ return $ Store TDouble 3
    ,   VMInstruction "astore_0"     75 $ return $ Store TRef 0
    ,   VMInstruction "astore_1"     76 $ return $ Store TRef 1
    ,   VMInstruction "astore_2"     77 $ return $ Store TRef 2
    ,   VMInstruction "astore_3"     78 $ return $ Store TRef 3

    ,   VMInstruction "iastore"      79 $ return $ AStore TInt
    ,   VMInstruction "lastore"      80 $ return $ AStore TLong
    ,   VMInstruction "fastore"      81 $ return $ AStore TFloat
    ,   VMInstruction "dastore"      82 $ return $ AStore TDouble
    ,   VMInstruction "aastore"      83 $ return $ AStore TRef
    ,   VMInstruction "bastore"      84 $ return $ AStore TByte
    ,   VMInstruction "castore"      85 $ return $ AStore TChar
    ,   VMInstruction "sastore"      86 $ return $ AStore TShort

    ,   VMInstruction "pop"          87 $ return Pop
    ,   VMInstruction "pop2"         88 $ return Pop2
    ,   VMInstruction "dup"          89 $ return Dup
    ,   VMInstruction "dup_x1"       90 $ return DupX1
    ,   VMInstruction "dup_x2"       91 $ return DupX2
    ,   VMInstruction "dup2"         92 $ return Dup2
    ,   VMInstruction "dup2_x1"      93 $ return Dup2X1
    ,   VMInstruction "dup2_x2"      94 $ return Dup2X2
    ,   VMInstruction "swap"         95 $ return Swap

    ,   VMInstruction "iadd"         96 $ return $ Add TInt
    ,   VMInstruction "ladd"         97 $ return $ Add TLong
    ,   VMInstruction "fadd"         98 $ return $ Add TFloat
    ,   VMInstruction "dadd"         99 $ return $ Add TDouble

    ,   VMInstruction "isub"        100 $ return $ Sub TInt
    ,   VMInstruction "lsub"        101 $ return $ Sub TLong
    ,   VMInstruction "fsub"        102 $ return $ Sub TFloat
    ,   VMInstruction "dsub"        103 $ return $ Sub TDouble

    ,   VMInstruction "imul"        104 $ return $ Mul TInt
    ,   VMInstruction "lmul"        105 $ return $ Mul TLong
    ,   VMInstruction "fmul"        106 $ return $ Mul TFloat
    ,   VMInstruction "dmul"        107 $ return $ Mul TDouble

    ,   VMInstruction "idiv"        108 $ return $ Div TInt
    ,   VMInstruction "ldiv"        109 $ return $ Div TLong
    ,   VMInstruction "fdiv"        110 $ return $ Div TFloat
    ,   VMInstruction "ddiv"        111 $ return $ Div TDouble


    ,   VMInstruction "irem"        112 $ return $ Rem TInt
    ,   VMInstruction "lrem"        113 $ return $ Rem TLong
    ,   VMInstruction "frem"        114 $ return $ Rem TFloat
    ,   VMInstruction "drem"        115 $ return $ Rem TDouble

    ,   VMInstruction "ineg"        116 $ return $ Neg TInt
    ,   VMInstruction "lneg"        117 $ return $ Neg TLong
    ,   VMInstruction "fneg"        118 $ return $ Neg TFloat
    ,   VMInstruction "dneg"        119 $ return $ Neg TDouble

    ,   VMInstruction "ishl"        120 $ return $ Shl TInt
    ,   VMInstruction "lshl"        121 $ return $ Shl TLong
    ,   VMInstruction "ishr"        122 $ return $ Shr TInt
    ,   VMInstruction "lshr"        123 $ return $ Shr TLong
    ,   VMInstruction "iushr"       124 $ return $ UShr TInt
    ,   VMInstruction "lushr"       125 $ return $ UShr TLong
    ,   VMInstruction "iand"        126 $ return $ And TInt
    ,   VMInstruction "land"        127 $ return $ And TLong
    ,   VMInstruction "ior"         128 $ return $ Or TInt
    ,   VMInstruction "lor"         129 $ return $ Or TLong
    ,   VMInstruction "ixor"        130 $ return $ XOr TInt
    ,   VMInstruction "lxor"        131 $ return $ XOr TLong

    ,   VMInstruction "iinc"        132 $ liftM2 IInc readByte readByte

    ,   VMInstruction "i2l"         133 $ return $ Coerce TInt TLong  
    ,   VMInstruction "i2f"         134 $ return $ Coerce TInt TFloat  
    ,   VMInstruction "i2d"         135 $ return $ Coerce TInt TDouble 
    ,   VMInstruction "l2i"         136 $ return $ Coerce TLong TInt
    ,   VMInstruction "l2f"         137 $ return $ Coerce TLong TFloat
    ,   VMInstruction "l2d"         138 $ return $ Coerce TLong TDouble
    ,   VMInstruction "f2i"         139 $ return $ Coerce TFloat TInt
    ,   VMInstruction "f2l"         140 $ return $ Coerce TFloat TLong
    ,   VMInstruction "f2d"         141 $ return $ Coerce TFloat TDouble
    ,   VMInstruction "d2i"         142 $ return $ Coerce TDouble TInt
    ,   VMInstruction "d2l"         143 $ return $ Coerce TDouble TLong
    ,   VMInstruction "d2f"         144 $ return $ Coerce TDouble TFloat
    ,   VMInstruction "i2b"         145 $ return $ Coerce TInt TByte
    ,   VMInstruction "i2c"         146 $ return $ Coerce TInt TChar
    ,   VMInstruction "i2s"         147 $ return $ Coerce TInt TShort


    ,   VMInstruction "lcmp"        148 $ return $ Cmp TLong
    ,   VMInstruction "fcmpl"       149 $ return $ Cmp TFloat   -- todo: g/l difference
    ,   VMInstruction "fcmpg"       150 $ return $ Cmp TFloat   -- todo: g/l difference
    ,   VMInstruction "dcmpl"       151 $ return $ Cmp TDouble  -- todo: g/l difference
    ,   VMInstruction "dcmpg"       152 $ return $ Cmp TDouble  -- todo: g/l difference

    ,   VMInstruction "ifeq"        153 $ GotoIf ZEq   `liftM` readShort
    ,   VMInstruction "ifne"        154 $ GotoIf ZNe   `liftM` readShort
    ,   VMInstruction "iflt"        155 $ GotoIf ZLt   `liftM` readShort
    ,   VMInstruction "ifge"        156 $ GotoIf ZGe   `liftM` readShort
    ,   VMInstruction "ifgt"        157 $ GotoIf ZGt   `liftM` readShort
    ,   VMInstruction "ifle"        158 $ GotoIf ZLe   `liftM` readShort
    ,   VMInstruction "if_icmpeq"   159 $ GotoIf Eq    `liftM` readShort
    ,   VMInstruction "if_icmpne"   160 $ GotoIf Ne    `liftM` readShort
    ,   VMInstruction "if_icmplt"   161 $ GotoIf Lt    `liftM` readShort
    ,   VMInstruction "if_icmpge"   162 $ GotoIf Ge    `liftM` readShort
    ,   VMInstruction "if_icmpgt"   163 $ GotoIf Gt    `liftM` readShort
    ,   VMInstruction "if_icmple"   164 $ GotoIf Le    `liftM` readShort
    ,   VMInstruction "if_acmpeq"   165 $ GotoIf RefEq `liftM` readShort
    ,   VMInstruction "if_acmpne"   166 $ GotoIf RefNe `liftM` readShort
    ,   VMInstruction "goto"        167 $ Goto `liftM` readShort
    ,   VMInstruction "jsr"         168 $ JSR  `liftM` readShort
    ,   VMInstruction "ret"         169 $ Ret  `liftM` readByte

    ,   VMInstruction "tableswitch"  170 $ tableswitch
    ,   VMInstruction "lookupswitch" 171 $ lookupswitch

    ,   VMInstruction "ireturn"     172 $ return $ Return $ Just TInt
    ,   VMInstruction "lreturn"     173 $ return $ Return $ Just TLong
    ,   VMInstruction "freturn"     174 $ return $ Return $ Just TFloat
    ,   VMInstruction "dreturn"     175 $ return $ Return $ Just TDouble
    ,   VMInstruction "areturn"     176 $ return $ Return $ Just TRef
    ,   VMInstruction "return"      177 $ return $ Return $ Nothing

    ,   VMInstruction "getstatic"   178 $ GetStatic `liftM` readFieldRef
    ,   VMInstruction "putstatic"   179 $ PutStatic `liftM` readFieldRef

    ,   VMInstruction "getfield"    180 $ GetField `liftM` readFieldRef
    ,   VMInstruction "putfield"    181 $ PutField `liftM` readFieldRef

    ,   VMInstruction "invokevirtual"   182 $ InvokeVirtual `liftM` readMethodRef
    ,   VMInstruction "invokespecial"   183 $ InvokeSpecial `liftM` readMethodRef
    ,   VMInstruction "invokestatic"    184 $ InvokeStatic  `liftM` readMethodRef
    ,   VMInstruction "invokeinterface" 185 $ do { ref <- readShort; lift $ skip 2; cp <- ask; return $ InvokeInterface $ getMethodRef cp ref }

    ,   VMInstruction "new"           187 $ New `liftM` readClassName

    ,   VMInstruction "newarray"      188 $ (ANew . Left  . fromAType)    `liftM` readByte
    ,   VMInstruction "anewarray"     189 $ (ANew . Right) `liftM` readClassName

    ,   VMInstruction "arraylength"   190 $ return ALength
    ,   VMInstruction "athrow"        191 $ return Throw

    ,   VMInstruction "checkcast"   192 $ CheckCast  `liftM` readClassName
    ,   VMInstruction "instanceof"  193 $ InstanceOf `liftM` readClassName

    ,   VMInstruction "monitorenter"  194 $ return MonitorEnter 
    ,   VMInstruction "monitorexit"   195 $ return MonitorExit

    ,   VMInstruction "wide"        196 $ parseWide

    ,   VMInstruction "multinewarray" 197 $ liftM2 MultiNew readArrayType (readByte)

    ,   VMInstruction "ifnull"      198 $ (GotoIf Null)    `liftM` readShort
    ,   VMInstruction "ifnonnull"   199 $ (GotoIf NotNull) `liftM` readShort
    ,   VMInstruction "goto_w"      200 $ Goto `liftM` readInt
    ,   VMInstruction "jsr_w"       201 $ JSR  `liftM` readInt] 
    where
        fromAType   :: Int -> VMOperandType
        fromAType  4 = TBoolean
        fromAType  5 = TChar 
        fromAType  6 = TFloat
        fromAType  7 = TDouble
        fromAType  8 = TByte
        fromAType  9 = TShort
        fromAType 10 = TInt
        fromAType 11 = TLong
        fromAType  i = error $ "invalid array type " ++ show i

        getArrayType :: ConstantPool -> Int -> ArrayType
        getArrayType cp i = uncurry ArrayType $ jtype2arraytype $ getJType cp i

        jtype2arraytype :: T.JType -> (Either VMOperandType String, Int)
        jtype2arraytype (T.TArray b) = case b of 
                                           T.TArray _    -> let (b', i) = jtype2arraytype b in (b', i + 1)
                                           T.TInstance i -> (Right i, 0)
                                           T.TByte       -> (Left TByte, 0)
                                           T.TChar       -> (Left TChar, 0)
                                           T.TDouble     -> (Left TDouble, 0)
                                           T.TFloat      -> (Left TFloat, 0)
                                           T.TInt        -> (Left TInt, 0)
                                           T.TLong       -> (Left TLong, 0)
                                           T.TShort      -> (Left TShort, 0)
                                           T.TBoolean    -> (Left TBoolean, 0)


        readShort :: Integral a => CPGet a
        readShort = lift (fromIntegral `liftM` (get :: Get Word16)) -- TODO [!!!] to utilities module 
        
        readByte :: Integral a => CPGet a
        readByte = lift (fromIntegral `liftM` (get :: Get Word8)) -- TODO [!!!] to utilities module 
        
        readInt :: Integral a => CPGet a
        readInt = lift (fromIntegral `liftM` (get :: Get Word32)) -- TODO [!!!] to utilities module 

        readFieldRef  = liftM2 getFieldRef ask readShort
        readMethodRef = liftM2 getMethodRef ask readShort
        readClassName = liftM2 getClassName ask readShort 
        readArrayType = liftM2 getArrayType ask readShort 

        pad :: Int64 -> CPGet ()
        pad n = do
            offs <- lift bytesRead
            let m = offs `mod` n
            lift $ skip $ if m == 0 then 0 else fromIntegral (n - m)

        lookupswitch = do
            pad 4
            def     <- readInt
            npairs  <- readInt
            pairs   <- replicateM npairs (liftM2 (,) readInt readInt)
            return $ LookupSwitch pairs def

        tableswitch = do
            pad 4
            def  <- readInt
            low  <- readInt
            high <- readInt
            offs <- replicateM (fromIntegral $ high - low + 1) readInt
            return $ TableSwitch low high offs

        parseWide = do
            opcode <- readByte
            case opcode of 
                -- iload, lload, fload, dload, aload    
                21 -> Load TInt    `liftM` readShort
                22 -> Load TLong   `liftM` readShort
                23 -> Load TFloat  `liftM` readShort
                24 -> Load TDouble `liftM` readShort
                25 -> Load TRef    `liftM` readShort
                -- istore, lstore, fstore, dstore, astore
                54 -> Store TInt    `liftM` readShort
                55 -> Store TLong   `liftM` readShort
                56 -> Store TFloat  `liftM` readShort
                57 -> Store TDouble `liftM` readShort
                58 -> Store TRef    `liftM` readShort
                -- ret
                169 -> Ret `liftM` readShort
                -- iinc
                132 -> liftM2 IInc readShort readShort
                _   -> error $ "invalid wide operation " ++ show opcode
                                                     
                                                     



        

    

