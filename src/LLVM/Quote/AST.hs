{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-} -- XXX: this is CRAZY
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LLVM.Quote.AST (
  Module(..),
  Definition(..),
  Global(..),
  Parameter(..),
  Direction(..),
  InstructionMetadata,
  LandingPadClause(..),
  FastMathFlags(..),
  Instruction(..),
  NamedInstruction(..),
  LabeledInstruction(..),
  A.MetadataNodeID(..),
  MetadataNode(..),
  Metadata(..),
  Operand(..),
  CallableOperand,
  Constant(..),
  Name(..),
  Type(..),
  InlineAssembly(..),
  DataLayout(..),
  TargetTriple(..),
  Extensions(..),
  ExtensionsInt
) where

import LLVM.Prelude
import qualified LLVM.AST.Operand as A
import qualified LLVM.AST.Float as A
import qualified LLVM.AST.Linkage as A
import qualified LLVM.AST.Visibility as A
import qualified LLVM.AST.CallingConvention as A
import qualified LLVM.AST.AddrSpace as A
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Instruction as A
import qualified LLVM.AST.InlineAssembly as A
import qualified LLVM.AST.IntegerPredicate as AI
import qualified LLVM.AST.FloatingPointPredicate as AF
import qualified LLVM.AST.RMWOperation as A
import qualified LLVM.AST.Type as A
import qualified LLVM.AST.DataLayout as A
import qualified LLVM.AST.ParameterAttribute as A
import qualified LLVM.AST.ThreadLocalStorage as TLS
import qualified LLVM.AST.DLL as DLL
import qualified LLVM.AST.Global as G

import Data.Word
import Data.Char (ord, chr)
import Data.ByteString.Short as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax (Lift(..))
import Instances.TH.Lift

data Extensions
  = Antiquotation
  | Loops
  deriving (Eq, Ord, Enum, Show)
type ExtensionsInt = Word32

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalValue.html>
data Global
    -- | <http://llvm.org/docs/LangRef.html#global-variables>
    = GlobalVariable {
        name :: Name,
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        dllStorageClass :: Maybe DLL.StorageClass,
        threadLocalMode :: Maybe TLS.Model,
        unnamedAddr :: Maybe G.UnnamedAddr,
        isConstant :: Bool,
        _type' :: Type,
        addrSpace :: A.AddrSpace,
        initializer :: Maybe Constant,
        section :: Maybe ShortByteString,
        comdat :: Maybe ShortByteString,
        alignment :: Word32
      }
    -- | <http://llvm.org/docs/LangRef.html#aliases>
    | GlobalAlias {
        name :: Name,
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        dllStorageClass :: Maybe DLL.StorageClass,
        threadLocalMode :: Maybe TLS.Model,
        unnamedAddr :: Maybe G.UnnamedAddr,
        _type' :: Type,
        addrSpace :: A.AddrSpace,
        aliasee :: Constant
      }
    -- | <http://llvm.org/docs/LangRef.html#functions>
    | Function {
        linkage :: A.Linkage,
        visibility :: A.Visibility,
        dllStorageClass :: Maybe DLL.StorageClass,
        _callingConvention :: A.CallingConvention,
        _returnAttributes :: [A.ParameterAttribute],
        returnType :: Type,
        name :: Name,
        parameters :: ([Parameter],Bool), -- ^ snd indicates varargs
        _functionAttributes :: [Either A.GroupID A.FunctionAttribute],
        section :: Maybe ShortByteString,
        comdat :: Maybe ShortByteString,
        alignment :: Word32,
        garbageCollectorName :: Maybe ShortByteString,
        prefix :: Maybe Constant,
        instructions :: [LabeledInstruction],
        _personalityFunction :: Maybe Constant
      }
  deriving (Eq, Read, Show, Typeable, Data)

-- | 'Parameter's for 'Function's
data Parameter
  = Parameter Type Name [A.ParameterAttribute]
  | AntiParameter ShortByteString
  | AntiParameterList ShortByteString
  deriving (Eq, Read, Show, Typeable, Data)

data Direction
  = Up
  | Down
  deriving (Eq, Read, Show, Typeable, Data)

-- | Any thing which can be at the top level of a 'Module'
data Definition
  = GlobalDefinition Global
  | TypeDefinition Name (Maybe Type)
  | MetadataNodeDefinition A.MetadataNodeID [Maybe Operand]
  | NamedMetadataDefinition ShortByteString [A.MetadataNodeID]
  | ModuleInlineAssembly ByteString
  | AntiDefinition ShortByteString
  | AntiDefinitionList ShortByteString
    deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#modulestructure>
data Module =
  Module {
    moduleName :: ShortByteString,
    moduleSourceFileName :: ShortByteString,
    -- | a 'DataLayout', if specified, must match that of the eventual code generator
    moduleDataLayout :: Maybe DataLayout,
    moduleTargetTriple :: TargetTriple,
    moduleDefinitions :: [Definition]
  }
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#metadata-nodes-and-metadata-strings>
-- Metadata can be attached to an instruction
type InstructionMetadata = [(ShortByteString, MetadataNode)]

-- | For the redoubtably complex 'LandingPad' instruction
data LandingPadClause
    = Catch Constant
    | Filter Constant
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#fast-math-flags>
data FastMathFlags
  = NoFastMathFlags
  | UnsafeAlgebra
  | FastMathFlags {
      noNaNs :: Bool,
      noInfs :: Bool,
      noSignedZeros :: Bool,
      allowReciprocal :: Bool
    }
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | non-terminator instructions:
-- <http://llvm.org/docs/LangRef.html#binaryops>
-- <http://llvm.org/docs/LangRef.html#bitwiseops>
-- <http://llvm.org/docs/LangRef.html#memoryops>
-- <http://llvm.org/docs/LangRef.html#otherops>
data Instruction
  = Add {
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FAdd {
      fastMathFlags :: FastMathFlags,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Sub {
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FSub {
      fastMathFlags :: FastMathFlags,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Mul {
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FMul {
      fastMathFlags :: FastMathFlags,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | UDiv {
      exact :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | SDiv {
      exact :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FDiv {
      fastMathFlags :: FastMathFlags,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | URem {
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | SRem {
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FRem {
      fastMathFlags :: FastMathFlags,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Shl {
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | LShr {
      exact :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | AShr {
      exact :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | And {
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Or {
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Xor {
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Alloca {
      allocatedType :: Type,
      numElements :: Maybe Operand,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | Load {
      volatile :: Bool,
      address :: Operand,
      maybeAtomicity :: Maybe A.Atomicity,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | Store {
      volatile :: Bool,
      address :: Operand,
      value :: Operand,
      maybeAtomicity :: Maybe A.Atomicity,
      alignmentI :: Word32,
      metadata :: InstructionMetadata
    }
  | GetElementPtr {
      inBounds :: Bool,
      address :: Operand,
      indices :: [Operand],
      metadata :: InstructionMetadata
    }
  | Fence {
      atomicity :: A.Atomicity,
      metadata :: InstructionMetadata
    }
  | CmpXchg {
      volatile :: Bool,
      address :: Operand,
      expected :: Operand,
      replacement :: Operand,
      atomicity :: A.Atomicity,
      failureMemoryOrdering :: A.MemoryOrdering,
      metadata :: InstructionMetadata
    }
  | AtomicRMW {
      volatile :: Bool,
      rmwOperation :: A.RMWOperation,
      address :: Operand,
      value :: Operand,
      atomicity :: A.Atomicity,
      metadata :: InstructionMetadata
    }
  | Trunc {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | ZExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | SExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPToUI {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPToSI {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | UIToFP {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | SIToFP {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPTrunc {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | PtrToInt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | IntToPtr {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | BitCast {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | AddrSpaceCast {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | ICmp {
      iPredicate :: AI.IntegerPredicate,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FCmp {
      fpPredicate :: AF.FloatingPointPredicate,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Phi {
      type' :: Type,
      incomingValues :: [ (Operand, Name) ],
      metadata :: InstructionMetadata
    }
  | Call {
      tailCallKind :: Maybe A.TailCallKind,
      callingConvention :: A.CallingConvention,
      returnAttributes :: [A.ParameterAttribute],
      function :: CallableOperand,
      arguments :: [(Operand, [A.ParameterAttribute])],
      functionAttributes :: [Either A.GroupID A.FunctionAttribute],
      metadata :: InstructionMetadata
  }
  | Select {
      condition' :: Operand,
      trueValue :: Operand,
      falseValue :: Operand,
      metadata :: InstructionMetadata
    }
  | VAArg {
      argList :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | ExtractElement {
      vector :: Operand,
      index :: Operand,
      metadata :: InstructionMetadata
    }
  | InsertElement {
      vector :: Operand,
      element :: Operand,
      index :: Operand,
      metadata :: InstructionMetadata
    }
  | ShuffleVector {
      operand0 :: Operand,
      operand1 :: Operand,
      mask :: Constant,
      metadata :: InstructionMetadata
    }
  | ExtractValue {
      aggregate :: Operand,
      indices' :: [Word32],
      metadata :: InstructionMetadata
    }
  | InsertValue {
      aggregate :: Operand,
      element :: Operand,
      indices' :: [Word32],
      metadata :: InstructionMetadata
    }
  | LandingPad {
      type' :: Type,
      cleanup :: Bool,
      clauses :: [LandingPadClause],
      metadata :: InstructionMetadata
    }
  | AntiInstruction ShortByteString
  | Ret {
      returnOperand :: Maybe Operand,
      metadata :: InstructionMetadata
    }
  | CondBr {
      condition :: Operand,
      trueDest :: Name,
      falseDest :: Name,
      metadata' :: InstructionMetadata
    }
  | Br {
      dest :: Name,
      metadata' :: InstructionMetadata
    }
  | Switch {
      operand0' :: Operand,
      defaultDest :: Name,
      dests :: [(Constant, Name)],
      metadata' :: InstructionMetadata
    }
  | IndirectBr {
      operand0' :: Operand,
      possibleDests :: [Name],
      metadata' :: InstructionMetadata
    }
  | Invoke {
      callingConvention' :: A.CallingConvention,
      returnAttributes' :: [A.ParameterAttribute],
      function' :: CallableOperand,
      arguments' :: [(Operand, [A.ParameterAttribute])],
      functionAttributes' :: [Either A.GroupID A.FunctionAttribute],
      returnDest :: Name,
      exceptionDest :: Name,
      metadata :: InstructionMetadata
    }
  | Resume {
      operand0 :: Operand,
      metadata :: InstructionMetadata
    }
  | Unreachable {
      metadata :: InstructionMetadata
    }
  | OperandInstruction Operand
  deriving (Eq, Read, Show, Typeable, Data)

data LabeledInstruction
  = Labeled {
    label :: Name,
    instruction :: NamedInstruction }
  | ForLoop {
    label :: Name,
    iterType :: Type,
    iterName :: Name,
    direction :: Direction,
    from :: Operand,
    to :: Operand,
    step :: Operand,
    body :: [LabeledInstruction]}
  | ITE {
    label :: Name,
    cond :: Operand,
    then_body :: [LabeledInstruction],
    else_body :: [LabeledInstruction]
    }
  | While {
    label :: Name,
    cond :: Operand,
    body :: [LabeledInstruction]
    }
  deriving (Eq, Read, Show, Typeable, Data)

-- | Instances of instructions may be given a name, allowing their results to be referenced as 'Operand's.
-- Sometimes instructions - e.g. a call to a function returning void - don't need names.
data NamedInstruction
  = Name := Instruction
  | Do Instruction
  | AntiInstructionList ShortByteString
  | AntiBasicBlock ShortByteString
  | AntiBasicBlockList ShortByteString
  deriving (Eq, Read, Show, Typeable, Data)

-- | An 'Operand' is roughly that which is an argument to an 'LLVM.AST.Instruction.Instruction'
data Operand
  -- | %foo
  = LocalReference Type Name
  -- | 'Constant's include 'LLVM.AST.Constant.GlobalReference', for \@foo
  | ConstantOperand Constant
  | MetadataOperand Metadata
  | AntiOperand ShortByteString
  deriving (Eq, Ord, Read, Show, Typeable, Data)

data MetadataNode
  = MetadataNode [Maybe Metadata]
  | MetadataNodeReference A.MetadataNodeID
  deriving (Eq, Ord, Read, Show, Typeable, Data)

data Metadata
  = MDString ShortByteString
  | MDNode MetadataNode
  | MDValue Operand
  deriving (Eq, Ord, Read, Show, Typeable, Data)


-- | The 'LLVM.AST.Instruction.Call' instruction is special: the callee can be inline assembly
type CallableOperand  = Either InlineAssembly Operand

{- |
<http://llvm.org/docs/LangRef.html#constants>

N.B. - <http://llvm.org/docs/LangRef.html#constant-expressions>

Although constant expressions and instructions have many similarites, there are important
differences - so they're represented using different types in this AST. At the cost of making it
harder to move an code back and forth between being constant and not, this approach embeds more of
the rules of what IR is legal into the Haskell types.
-}
data Constant
    = Int { integerBits :: Word32, integerValue :: Integer }
    | IntAntiBs { antiIntegerBits :: ShortByteString, integerValue :: Integer }
    | Float { floatValue :: A.SomeFloat }
    | Null { constantType :: Type }
    | Struct { structName :: Maybe Name, _isPacked :: Bool, memberValues :: [ Constant ] }
    | Array { memberType :: Type, memberValues :: [ Constant ] }
    | Vector { memberValues :: [ Constant ] }
    | Undef { constantType :: Type }
    | BlockAddress { blockAddressFunction :: Name, blockAddressBlock :: Name }
    | GlobalReference Type Name
    | AntiConstant ShortByteString
    deriving (Eq, Ord, Read, Show, Typeable, Data)

{- |
Objects of various sorts in LLVM IR are identified by address in the LLVM C++ API, and
may be given a string name. When printed to (resp. read from) human-readable LLVM assembly, objects without
string names are numbered sequentially (resp. must be numbered sequentially). String names may be quoted, and
are quoted when printed if they would otherwise be misread - e.g. when containing special characters.

> 7

means the seventh unnamed object, while

> "7"

means the object named with the string "7".

This libraries handling of 'UnName's during translation of the AST down into C++ IR is somewhat more
forgiving than the LLVM assembly parser: it does not require that unnamed values be numbered sequentially;
however, the numbers of 'UnName's passed into C++ cannot be preserved in the C++ objects. If the C++ IR is
printed as assembly or translated into a Haskell AST, unnamed nodes will be renumbered sequentially. Thus
unnamed node numbers should be thought of as having any scope limited to the 'LLVM.AST.Module' in
which they are used.
-}
data Name
    = Name ShortByteString -- ^ a string name
    | UnName Word -- ^ a number for a nameless thing
    | NeedsName
    | AntiName ShortByteString
   deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#type-system>
data Type
  -- | <http://llvm.org/docs/LangRef.html#void-type>
  = VoidType
  -- | <http://llvm.org/docs/LangRef.html#integer-type>
  | IntegerType { typeBits :: Word32 }
  -- | <http://llvm.org/docs/LangRef.html#pointer-type>
  | PointerType { pointerReferent :: Type, pointerAddrSpace :: A.AddrSpace }
  -- | <http://llvm.org/docs/LangRef.html#floating-point-types>
  | FloatingPointType { floatingPointType :: A.FloatingPointType }
  -- | <http://llvm.org/docs/LangRef.html#function-type>
  | FunctionType { resultType :: Type, argumentTypes :: [Type], isVarArg :: Bool }
  -- | <http://llvm.org/docs/LangRef.html#vector-type>
  | VectorType { nVectorElements :: Word32, elementType :: Type }
  -- | <http://llvm.org/docs/LangRef.html#structure-type>
  | StructureType { isPacked :: Bool, elementTypes :: [Type] }
  -- | <http://llvm.org/docs/LangRef.html#array-type>
  | ArrayType { nArrayElements :: Word64, elementType :: Type }
  -- | <http://llvm.org/docs/LangRef.html#opaque-structure-types>
  | NamedTypeReference Name
  -- | <http://llvm.org/docs/LangRef.html#metadata-type>
  | MetadataType -- only to be used as a parameter type for a few intrinsics
  | AntiType ShortByteString
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#inline-assembler-expressions>
-- to be used through 'LLVM.AST.Operand.CallableOperand' with a
-- 'LLVM.AST.Instruction.Call' instruction
data InlineAssembly
  = InlineAssembly {
      __type' :: Type,
      assembly :: ByteString,
      constraints :: ShortByteString,
      hasSideEffects :: Bool,
      alignStack :: Bool,
      dialect :: A.Dialect
    }
  deriving (Eq, Read, Show, Typeable, Data)

-- | a description of the various data layout properties which may be used during
-- optimization
data DataLayout
  = DataLayout {
    endianness :: A.Endianness,
    mangling :: Maybe A.Mangling,
    stackAlignment :: Maybe Word32,
    pointerLayouts :: M.Map A.AddrSpace (Word32, A.AlignmentInfo),
    typeLayouts :: M.Map (A.AlignType, Word32) A.AlignmentInfo,
    aggregateLayout :: A.AlignmentInfo,
    nativeSizes :: Maybe (S.Set Word32)
  }
  | AntiDataLayout ShortByteString
  deriving (Eq, Ord, Read, Show, Typeable, Data)

data TargetTriple
  = NoTargetTriple
  | TargetTriple ShortByteString
  | AntiTargetTriple ShortByteString
  deriving (Eq, Ord, Read, Show, Typeable, Data)

deriving instance Lift A.Mangling
deriving instance Lift DLL.StorageClass
deriving instance Lift TLS.Model
deriving instance Lift G.UnnamedAddr
deriving instance Lift A.Visibility
deriving instance Lift A.Linkage
deriving instance Lift A.ParameterAttribute
deriving instance Lift Global
deriving instance Lift Constant
deriving instance Lift A.AddrSpace
deriving instance Lift A.CallingConvention
deriving instance Lift A.FunctionAttribute
deriving instance Lift A.SomeFloat
deriving instance Lift AI.IntegerPredicate
deriving instance Lift AF.FloatingPointPredicate
deriving instance Lift Direction
deriving instance Lift Parameter
deriving instance Lift NamedInstruction
deriving instance Lift LabeledInstruction
deriving instance Lift Instruction
deriving instance Lift A.SynchronizationScope
deriving instance Lift InlineAssembly
deriving instance Lift A.Dialect
deriving instance Lift A.RMWOperation
deriving instance Lift A.Atomicity
deriving instance Lift LandingPadClause
deriving instance Lift A.MemoryOrdering
deriving instance Lift Name
deriving instance Lift MetadataNode
deriving instance Lift A.MetadataNodeID
instance Lift Operand where
  lift (LocalReference ty n) = [| LocalReference ty n |]
  lift (ConstantOperand c) = [| ConstantOperand c |]
  lift (MetadataOperand md) = [| MetadataOperand md |]
instance Lift Metadata where
  lift (MDString s) = [| MDString s |]
  lift (MDNode node) = [| MDNode node |]
  lift (MDValue o) = [| MDValue o |]
deriving instance Lift Type
deriving instance Lift A.FloatingPointType
deriving instance Lift DataLayout
deriving instance Lift A.Endianness
deriving instance Lift A.AlignType
deriving instance Lift A.AlignmentInfo
deriving instance Lift Definition
deriving instance Lift Module
deriving instance Lift TargetTriple
deriving instance Lift FastMathFlags
deriving instance Lift A.GroupID
deriving instance Lift A.TailCallKind

instance Lift ShortByteString where
  lift b = [| fromString $(lift (unpack b)) |]
    where
      unpack' :: BS.ShortByteString -> [Char]
      unpack' x = fmap (chr . fromIntegral) (BS.unpack x)

{-$(deriveLiftMany [''A.Visibility,-}
                  {-''A.Linkage,-}
                  {-''A.ParameterAttribute,-}
                  {-''Global,-}
                  {-''Constant,-}
                  {-''A.AddrSpace,-}
                  {-''A.CallingConvention,-}
                  {-''A.FunctionAttribute,-}
                  {-''A.SomeFloat,-}
                  {-''AI.IntegerPredicate,-}
                  {-''AF.FloatingPointPredicate,-}
                  {-''Direction,-}
                  {-''Parameter,-}
                  {-''NamedInstruction,-}
                  {-''LabeledInstruction,-}
                  {-''Instruction,-}
                  {-''A.SynchronizationScope,-}
                  {-''InlineAssembly,-}
                  {-''A.Dialect,-}
                  {-''A.RMWOperation,-}
                  {-''A.Atomicity,-}
                  {-''LandingPadClause,-}
                  {-''A.MemoryOrdering,-}
                  {-''Name,-}
                  {-''MetadataNode,-}
                  {-''MetadataNodeID,-}
                  {-''Operand,-}
                  {-''Type,-}
                  {-''A.FloatingPointType,-}
                  {-''DataLayout,-}
                  {-''A.Endianness,-}
                  {-''M.Map,-}
                  {-''A.AlignType,-}
                  {-''A.AlignmentInfo,-}
                  {-''S.Set,-}
                  {-''Definition,-}
                  {-''Module,-}
                  {-''TargetTriple,-}
                  {-''FastMathFlags-}
                  {-])-}

{-instance Lift Word64 where-}
  {-lift = lift . toInteger-}
{-instance Lift Word32 where-}
  {-lift = lift . toInteger-}
{-instance Lift Word16 where-}
  {-lift = lift . toInteger-}
{-instance Lift Word where-}
  {-lift = lift . toInteger-}
{-instance Lift Float where-}
  {-lift = lift . toRational-}
{-instance Lift Double where-}
  {-lift = lift . toRational-}
