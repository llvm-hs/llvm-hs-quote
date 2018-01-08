{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module LLVM.Quote.Base (
    CodeGen,
    CodeGenMonad(..),
    ToDefintions(..),
    quasiquote,
    quasiquoteM,
    TQuasiQuoter(..),
    parse
  ) where

import Control.Monad.Identity
import qualified Data.ByteString.Char8 as B
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Word
import Data.Loc
import Data.Data (Data(..))
import Data.String (fromString)
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.IORef (atomicModifyIORef')
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified LLVM.Quote.Parser as P
import qualified LLVM.Quote.AST as A
import LLVM.Quote.SSA
import qualified LLVM.AST.IntegerPredicate as LI
import qualified LLVM.AST as L
import qualified LLVM.AST.Constant as L
  (Constant(Int, Float, Null, Struct, Array, Vector,
            Undef, BlockAddress, GlobalReference))
import qualified LLVM.AST.Float as L
import qualified LLVM.AST.InlineAssembly as L
import qualified LLVM.AST.DataLayout as L
import qualified LLVM.AST.Attribute as L

import qualified Data.Map as M

class (Applicative m, Monad m) => CodeGenMonad m where
  newVariable    :: m L.Name
  exec     :: m () -> m [L.BasicBlock]

type CodeGen = State (Int, M.Map L.Name [L.Operand])

instance CodeGenMonad CodeGen where
  newVariable      = state $ \(i,vs) -> (L.UnName (fromIntegral i), (i+1,vs))
  exec     = error "not defined: exec"

class ToBasicBlockList a where
  toBasicBlockList :: CodeGenMonad m => m a -> m [L.BasicBlock]
instance ToBasicBlockList () where
  toBasicBlockList = exec
instance ToBasicBlockList [L.BasicBlock] where
  toBasicBlockList = id

class ToDefintion a where
  toDefinition :: a -> L.Definition
instance ToDefintion L.Definition where
  toDefinition = id
instance ToDefintion L.Global where
  toDefinition = L.GlobalDefinition

class ToDefintions a where
  toDefinitions :: a -> [L.Definition]
instance ToDefintion a => ToDefintions [a] where
  toDefinitions = map toDefinition

class ToConstant a where
  toConstant :: a -> L.Constant
instance ToConstant L.Constant where
  toConstant = id
instance ToConstant Word8 where
  toConstant n = L.Int 8 (toInteger n)
instance ToConstant Word16 where
  toConstant n = L.Int 16 (toInteger n)
instance ToConstant Word32 where
  toConstant n = L.Int 32 (toInteger n)
instance ToConstant Word64 where
  toConstant n = L.Int 64 (toInteger n)
instance ToConstant Float where
  toConstant n = L.Float (L.Single n)
instance ToConstant Double where
  toConstant n = L.Float (L.Double n)

class ToName a where
  toName :: a -> L.Name
instance ToName L.Name where
  toName = id
instance ToName String where
  toName = L.Name . fromString
instance ToName Word where
  toName = L.UnName

class ToTargetTriple a where
  toTargetTriple :: a -> Maybe String
instance ToTargetTriple String where
  toTargetTriple = Just
instance ToTargetTriple (Maybe String) where
  toTargetTriple = id

-- TODO handle non ascii bytestrings
toString :: ShortByteString -> String
toString = B.unpack . fromShort

antiVarE :: ShortByteString -> ExpQ
antiVarE s = [|$(either fail return $ parseExp (toString s))|]

type Conversion a b = forall m.(CodeGenMonad m) => a -> TExpQ (m b)
type Conversion' m a b = (CodeGenMonad m) => a -> TExpQ (m b)

class QQExp a b where
  qqExpM :: Conversion a b
  qqExp :: a -> TExpQ b
  qqExp x = [||fst $ runState $$(qqExpM x) ((0,M.empty) :: (Int,M.Map L.Name [L.Operand]))||]

instance (Lift a) => QQExp a a where
  qqExpM x = [||pure x||]

instance QQExp A.InstructionMetadata L.InstructionMetadata where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [(A.Constant, A.Name)] [(L.Constant, L.Name)] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.Name] [L.Name] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [(A.Operand, [L.ParameterAttribute])]
               [(L.Operand, [L.ParameterAttribute])] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.Operand] [L.Operand] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [(A.Operand, A.Name)] [(L.Operand, L.Name)] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.LandingPadClause] [L.LandingPadClause] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [Maybe A.Operand] [Maybe L.Operand] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [Maybe A.Metadata] [Maybe L.Metadata] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.Constant] [L.Constant] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

instance QQExp [A.Type] [L.Type] where
  qqExpM (x:xs) = [||(:) <$> $$(qqExpM x) <*> $$(qqExpM xs)||]
  qqExpM []     = [||pure []||]

-- instance (QQExp a b) => QQExp (Maybe a) (Maybe b) where
--   qqExpM Nothing  = [||pure Nothing||]
--   qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance QQExp (Maybe A.Operand) (Maybe L.Operand) where
  qqExpM Nothing  = [||pure Nothing||]
  qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance QQExp (Maybe A.Metadata) (Maybe L.Metadata) where
  qqExpM Nothing  = [||pure Nothing||]
  qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance QQExp (Maybe A.Name) (Maybe L.Name) where
  qqExpM Nothing  = [||pure Nothing||]
  qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance QQExp (Maybe A.Type) (Maybe L.Type) where
  qqExpM Nothing  = [||pure Nothing||]
  qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance QQExp (Maybe A.DataLayout) (Maybe L.DataLayout) where
  qqExpM Nothing  = [||pure Nothing||]
  qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance QQExp (Maybe A.Constant) (Maybe L.Constant) where
  qqExpM Nothing  = [||pure Nothing||]
  qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance QQExp (Maybe (A.Type, A.Operand, A.Name))
               (Maybe (L.Type, L.Operand, L.Name)) where
  qqExpM Nothing  = [||pure Nothing||]
  qqExpM (Just x) = [||Just <$> $$(qqExpM x)||]

instance (QQExp a c, QQExp b d) => QQExp (Either a b) (Either c d) where
  qqExpM (Left x)  = [||Left <$> $$(qqExpM x)||]
  qqExpM (Right x) = [||Right <$> $$(qqExpM x)||]

instance (QQExp a c, QQExp b d) => QQExp (a,b) (c,d) where
  qqExpM (x,y) = [||(,) <$> $$(qqExpM x) <*> $$(qqExpM y)||]

instance (QQExp a d, QQExp b e, QQExp c f) => QQExp (a,b,c) (d,e,f) where
  qqExpM (x,y,z) = [||(,,) <$> $$(qqExpM x) <*> $$(qqExpM y) <*> $$(qqExpM z)||]

instance QQExp A.Definition L.Definition where
  qqExpM = qqDefinitionE
instance QQExp [A.Definition] [L.Definition] where
  qqExpM = qqDefinitionListE
instance QQExp A.Module L.Module where
  qqExpM = qqModuleE
instance QQExp A.Global L.Global where
  qqExpM = qqGlobalE
instance QQExp [A.Parameter] [L.Parameter] where
  qqExpM = qqParameterListE
instance QQExp A.Parameter L.Parameter where
  qqExpM = qqParameterE
instance QQExp A.LandingPadClause L.LandingPadClause where
  qqExpM = qqLandingPadClauseE
instance QQExp A.FastMathFlags L.FastMathFlags where
  qqExpM = qqFastMathFlagsE
instance QQExp A.InlineAssembly L.InlineAssembly where
  qqExpM = qqInlineAssemblyE
instance QQExp A.Instruction (Either L.Instruction L.Terminator) where
  qqExpM = qqInstructionE
instance QQExp A.Instruction L.Instruction where
  qqExpM x1 = [||do x1' <- $$(qqExpM x1)
                    case x1' :: Either L.Instruction L.Terminator of
                      Left  x1'' -> return x1''
                      Right x1'' -> fail $ show x1'' ++ " is no Instruction"||]
instance QQExp [A.LabeledInstruction] [L.BasicBlock] where
  qqExpM = qqLabeledInstructionListE
instance QQExp A.NamedInstruction [L.BasicBlock] where
  qqExpM = qqNamedInstructionE
instance QQExp A.LabeledInstruction [L.BasicBlock] where
  qqExpM = qqLabeledInstructionE
instance QQExp A.MetadataNode L.MetadataNode where
  qqExpM = qqMetadataNodeE
instance QQExp A.Metadata L.Metadata where
  qqExpM = qqMetadata
instance QQExp A.Operand L.Operand where
  qqExpM = qqOperandE
instance QQExp A.Constant L.Constant where
  qqExpM = qqConstantE
instance QQExp A.Name L.Name where
  qqExpM = qqNameE
instance QQExp A.Type L.Type where
  qqExpM = qqTypeE
instance QQExp A.DataLayout L.DataLayout where
  qqExpM = qqDataLayoutE
instance QQExp A.TargetTriple (Maybe ShortByteString) where
  qqExpM = qqTargetTripleE

qqDefinitionListE :: Conversion [A.Definition] [L.Definition]
qqDefinitionListE [] = [||pure []||]
qqDefinitionListE (A.AntiDefinitionList v : defs) =
    [||(++) <$> $$(unsafeTExpCoerce [|$(antiVarE v) >>= return . toDefinitions|])
       <*> $$(qqExpM defs)||]
qqDefinitionListE (def : defs) =
    [||(:) <$> $$(qqExpM def) <*> $$(qqExpM defs)||]

qqDefinitionE :: Conversion A.Definition L.Definition
qqDefinitionE (A.GlobalDefinition v) =
    [||L.GlobalDefinition <$> $$(qqExpM v)||]
qqDefinitionE (A.TypeDefinition n v) =
    [||L.TypeDefinition <$> $$(qqExpM n) <*> $$(qqExpM v)||]
qqDefinitionE (A.MetadataNodeDefinition i vs) =
    [||L.MetadataNodeDefinition <$> $$(qqExpM i) <*> $$(qqExpM vs)||]
qqDefinitionE (A.NamedMetadataDefinition i vs) =
    [||L.NamedMetadataDefinition <$> $$(qqExpM i) <*> $$(qqExpM vs)||]
qqDefinitionE (A.ModuleInlineAssembly s) =
    [||L.ModuleInlineAssembly <$> $$(qqExpM s)||]
qqDefinitionE (A.AntiDefinition s) =
    unsafeTExpCoerce $ [|$(antiVarE s) >>= return . toDefinition|]
qqDefinitionE a@(A.AntiDefinitionList _s) =
    error $ "Internal Error: unexpected antiquote " ++ show a

qqModuleE :: Conversion A.Module L.Module
qqModuleE (A.Module n fn dl tt ds) =
  [||L.Module <$> $$(qqExpM n) <*> $$(qqExpM fn) <*> $$(qqExpM dl) <*> $$(qqExpM tt) <*> $$(qqExpM ds)||]

qqGlobalE :: Conversion A.Global L.Global
qqGlobalE (A.GlobalVariable x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD) =
  [||L.GlobalVariable <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                      <*> $$(qqExpM x5) <*> $$(qqExpM x6) <*> $$(qqExpM x7) <*> $$(qqExpM x8)
                      <*> $$(qqExpM x9) <*> $$(qqExpM xA) <*> $$(qqExpM xB) <*> $$(qqExpM xC)
                      <*> $$(qqExpM xD)||]
qqGlobalE (A.GlobalAlias x1 x2 x3 x4 x5 x6 x7 x8 x9) =
  [||L.GlobalAlias <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                   <*> $$(qqExpM x5) <*> $$(qqExpM x6) <*> $$(qqExpM x7) <*> $$(qqExpM x8)
                   <*> $$(qqExpM x9)||]
qqGlobalE (A.Function x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF xG) =
  [||L.Function <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                <*> $$(qqExpM x5) <*> $$(qqExpM x6) <*> $$(qqExpM x7) <*> $$(qqExpM x8)
                <*> $$(qqExpM x9) <*> $$(qqExpM xA) <*> $$(qqExpM xB) <*> $$(qqExpM xC)
                <*> $$(qqExpM xD) <*> $$(qqExpM xE) <*> toSSA `fmap` $$(qqExpM xF) <*> $$(qqExpM xG)||]

qqParameterListE :: Conversion [A.Parameter] [L.Parameter]
qqParameterListE [] = [||pure []||]
qqParameterListE (A.AntiParameterList v : defs) =
    [||(++) <$> $$(unsafeTExpCoerce $ antiVarE v) <*> $$(qqExpM defs)||]
qqParameterListE (def : defs) =
    [||(:) <$> $$(qqExpM def) <*> $$(qqExpM defs)||]

qqParameterE :: Conversion A.Parameter L.Parameter
qqParameterE (A.Parameter x1 x2 x3) =
  [||L.Parameter <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqParameterE (A.AntiParameter s) =
  unsafeTExpCoerce $ antiVarE s
qqParameterE a@(A.AntiParameterList _s) =
  error $ "Internal Error: unexpected antiquote " ++ show a

qqLandingPadClauseE :: Conversion A.LandingPadClause L.LandingPadClause
qqLandingPadClauseE (A.Catch x1) =
  [||L.Catch <$> $$(qqExpM x1)||]
qqLandingPadClauseE (A.Filter x1) =
  [||L.Filter <$> $$(qqExpM x1)||]

qqFastMathFlagsE :: Conversion A.FastMathFlags L.FastMathFlags
qqFastMathFlagsE A.NoFastMathFlags =
  [||pure L.NoFastMathFlags||]
qqFastMathFlagsE A.UnsafeAlgebra =
  [||pure L.UnsafeAlgebra||]
qqFastMathFlagsE (A.FastMathFlags x1 x2 x3 x4) =
  [||L.FastMathFlags <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)||]

qqInlineAssemblyE :: Conversion A.InlineAssembly L.InlineAssembly
qqInlineAssemblyE (A.InlineAssembly x1 x2 x3 x4 x5 x6) =
  [||L.InlineAssembly <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                      <*> $$(qqExpM x5) <*> $$(qqExpM x6)||]

qqInstructionE :: Conversion A.Instruction (Either L.Instruction L.Terminator)
qqInstructionE (A.Add x1 x2 x3 x4 x5) =
  [||Left <$> (L.Add <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5))||]
qqInstructionE (A.FAdd x1 x2 x3 x4) =
  [||Left <$> (L.FAdd <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.Sub x1 x2 x3 x4 x5) =
  [||Left <$> (L.Sub <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5))||]
qqInstructionE (A.FSub x1 x2 x3 x4) =
  [||Left <$> (L.FSub <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.Mul x1 x2 x3 x4 x5) =
  [||Left <$> (L.Mul <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5))||]
qqInstructionE (A.FMul x1 x2 x3 x4) =
  [||Left <$> (L.FMul <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.UDiv x1 x2 x3 x4) =
  [||Left <$> (L.UDiv <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.SDiv x1 x2 x3 x4) =
  [||Left <$> (L.SDiv <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.FDiv x1 x2 x3 x4) =
  [||Left <$> (L.FDiv <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.URem x1 x2 x3) =
  [||Left <$> (L.URem <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.SRem x1 x2 x3) =
  [||Left <$> (L.SRem <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.FRem x1 x2 x3 x4) =
  [||Left <$> (L.FRem <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.Shl x1 x2 x3 x4 x5) =
  [||Left <$> (L.Shl <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5))||]
qqInstructionE (A.LShr x1 x2 x3 x4) =
  [||Left <$> (L.LShr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.AShr x1 x2 x3 x4) =
  [||Left <$> (L.AShr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.And x1 x2 x3) =
  [||Left <$> (L.And <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.Or x1 x2 x3) =
  [||Left <$> (L.Or <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.Xor x1 x2 x3) =
  [||Left <$> (L.Xor <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.Alloca x1 x2 x3 x4) =
  [||Left <$> (L.Alloca <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.Load x1 x2 x3 x4 x5) =
  [||Left <$> (L.Load <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5))||]
qqInstructionE (A.Store x1 x2 x3 x4 x5 x6) =
  [||Left <$> (L.Store <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)
             <*> $$(qqExpM x6))||]
qqInstructionE (A.GetElementPtr x1 x2 x3 x4) =
  [||Left <$> (L.GetElementPtr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.Fence x1 x2) =
  [||Left <$> (L.Fence <$> pure x1 <*> $$(qqExpM x2))||]
qqInstructionE (A.CmpXchg x1 x2 x3 x4 x5 x6 x7) =
  [||Left <$> (L.CmpXchg <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> pure x5
               <*> $$(qqExpM x6) <*> $$(qqExpM x7))||]
qqInstructionE (A.AtomicRMW x1 x2 x3 x4 x5 x6) =
  [||Left <$> (L.AtomicRMW <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4)
                 <*> pure x5 <*> $$(qqExpM x6))||]
qqInstructionE (A.Trunc x1 x2 x3) =
  [||Left <$> (L.Trunc <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.ZExt x1 x2 x3) =
  [||Left <$> (L.ZExt <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.SExt x1 x2 x3) =
  [||Left <$> (L.SExt <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.FPToUI x1 x2 x3) =
  [||Left <$> (L.FPToUI <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.FPToSI x1 x2 x3) =
  [||Left <$> (L.FPToSI <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.UIToFP x1 x2 x3) =
  [||Left <$> (L.UIToFP <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.SIToFP x1 x2 x3) =
  [||Left <$> (L.SIToFP <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.FPTrunc x1 x2 x3) =
  [||Left <$> (L.FPTrunc <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.FPExt x1 x2 x3) =
  [||Left <$> (L.FPExt <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.PtrToInt x1 x2 x3) =
  [||Left <$> (L.PtrToInt <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.IntToPtr x1 x2 x3) =
  [||Left <$> (L.IntToPtr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.BitCast x1 x2 x3) =
  [||Left <$> (L.BitCast <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.AddrSpaceCast x1 x2 x3) =
  [||Left <$> (L.AddrSpaceCast <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.ICmp x1 x2 x3 x4) =
  [||Left <$> (L.ICmp <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.FCmp x1 x2 x3 x4) =
  [||Left <$> (L.FCmp <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.Phi x1 x2 x3) =
  [||Left <$> (L.Phi <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.Call x1 x2 x3 x4 x5 x6 x7) =
  [||Left <$> (L.Call <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)
            <*> $$(qqExpM x6) <*> $$(qqExpM x7))||]
qqInstructionE (A.Select x1 x2 x3 x4) =
  [||Left <$> (L.Select <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.VAArg x1 x2 x3) =
  [||Left <$> (L.VAArg <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.ExtractElement x1 x2 x3) =
  [||Left <$> (L.ExtractElement <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.InsertElement x1 x2 x3 x4) =
  [||Left <$> (L.InsertElement <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.ShuffleVector x1 x2 x3 x4) =
  [||Left <$> (L.ShuffleVector <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.ExtractValue x1 x2 x3) =
  [||Left <$> (L.ExtractValue <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.InsertValue x1 x2 x3 x4) =
  [||Left <$> (L.InsertValue <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.LandingPad x1 x2 x3 x4) =
  [||Left <$> (L.LandingPad <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.OperandInstruction x1) =
  [||do x1' <- $$(qqExpM x1)
        let true = L.ConstantOperand $ L.Int 1 1
        return $ Left $ L.Select true x1' x1' []||]
qqInstructionE (A.AntiInstruction s) =
  unsafeTExpCoerce $ antiVarE s
qqInstructionE (A.Ret x1 x2) =
  [||Right <$> (L.Ret <$> $$(qqExpM x1) <*> $$(qqExpM x2))||]
qqInstructionE (A.CondBr x1 x2 x3 x4) =
  [||Right <$> (L.CondBr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.Br x1 x2) =
  [||Right <$> (L.Br <$> $$(qqExpM x1) <*> $$(qqExpM x2))||]
qqInstructionE (A.Switch x1 x2 x3 x4) =
  [||Right <$> (L.Switch <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4))||]
qqInstructionE (A.IndirectBr x1 x2 x3) =
  [||Right <$> (L.IndirectBr <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3))||]
qqInstructionE (A.Invoke x1 x2 x3 x4 x5 x6 x7 x8) =
  [||Right <$> (L.Invoke <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3) <*> $$(qqExpM x4) <*> $$(qqExpM x5)
              <*> $$(qqExpM x6) <*> $$(qqExpM x7) <*> $$(qqExpM x8))||]
qqInstructionE (A.Resume x1 x2) =
  [||Right <$> (L.Resume <$> $$(qqExpM x1) <*> $$(qqExpM x2))||]
qqInstructionE (A.Unreachable x1) =
  [||Right <$> (L.Unreachable <$> $$(qqExpM x1))||]

qqLabeledInstructionListE :: Conversion [A.LabeledInstruction] [L.BasicBlock]
qqLabeledInstructionListE [] =
  [||pure []||]
qqLabeledInstructionListE (x:xs) =
  [||let nextLabel :: L.Name
         nextLabel = L.Name "nextblock"

         jumpNext :: L.BasicBlock -> Bool
         jumpNext (L.BasicBlock _ _ t) =
           case t of
             _ L.:= L.Br l2 _ | l2 == nextLabel -> True
             L.Do (L.Br l2 _) | l2 == nextLabel -> True
             _                                   -> False

         replacePhiFroms :: [(L.Name,L.Name)] -> L.BasicBlock -> L.BasicBlock
         replacePhiFroms labels (L.BasicBlock n is t) =
           L.BasicBlock n (map (replacePhiFrom labels) is) t

         replacePhiFrom :: [(L.Name,L.Name)] -> L.Named L.Instruction -> L.Named L.Instruction
         replacePhiFrom names (n L.:= phi@L.Phi{}) =
           n L.:= replacePhiFrom' names phi
         replacePhiFrom names (L.Do phi@L.Phi{}) =
           L.Do $ replacePhiFrom' names phi
         replacePhiFrom _ named = named

         replacePhiFrom' :: [(L.Name,L.Name)] -> L.Instruction -> L.Instruction
         replacePhiFrom' names phi@L.Phi{} =
           phi{ L.incomingValues =
                   [ (op,n') | (op,n) <- L.incomingValues phi,
                               let n' = maybe n id (lookup n names)] }
         replacePhiFrom' _ _ =
           error "this should never happen"

         fuse :: L.BasicBlock -> L.BasicBlock -> Writer [(L.Name,L.Name)] L.BasicBlock
         fuse (L.BasicBlock n1 i1 _t1) (L.BasicBlock n2 i2 t2) = do
           tell [(n2,n1)]
           return $ L.BasicBlock n1 (i1++i2) t2

         fuseBlocks' :: [L.BasicBlock] -> Writer [(L.Name,L.Name)] [L.BasicBlock]
         fuseBlocks' bbs@[] = return bbs
         fuseBlocks' bbs@[_] = return bbs
         fuseBlocks' (bb1:bbs@(bb2:bbs')) =
           case jumpNext bb1 of
             True  -> do
               fused <- fuse bb1 bb2
               fuseBlocks' (fused:bbs')
             False -> do
               bbs_ <- fuseBlocks' bbs
               return $ bb1 : bbs_

         fuseBlocks :: [L.BasicBlock] -> [L.BasicBlock]
         fuseBlocks bbs =
           let (bbs',labels) = runWriter $ fuseBlocks' bbs
           in map (replacePhiFroms labels) bbs'

     in fuseBlocks <$> ((++) <$> $$(qqExpM x) <*> $$(qqExpM xs))||]

qqLabeledInstructionE :: forall m. Conversion' m A.LabeledInstruction [L.BasicBlock]
qqLabeledInstructionE (A.Labeled label instr) =
  [||do label' <- $$(qqExpM label)
        L.BasicBlock _ is t:bbs <- $$(qqExpM instr)
        return $ L.BasicBlock label' is t:bbs||]
qqLabeledInstructionE (A.ForLoop label iterType iterName direction from to step body) =
  [||do
    label' <- $$(qqExpM label)
    body' <- $$(qqExpM body :: TExpQ (m [L.BasicBlock]))
    iterName' <- $$(qqExpM iterName :: TExpQ (m L.Name))
    iterType' <- $$(qqExpM iterType :: TExpQ (m L.Type))
    from' <- $$(qqExpM from :: TExpQ (m L.Operand))
    to' <- $$(qqExpM to)
    step' <- $$(qqExpM step :: TExpQ (m L.Operand))

    let labelString = case label' of
                        L.Name s -> s
                        L.UnName n -> fromString ("num" <> show n)
        cond = L.Name (labelString <> ".cond")
        labelHead = L.Name (labelString <> ".head")
        labelEnd = L.Name (labelString <> ".end")
        labelLast = L.Name (labelString <> ".last")

        iter = L.LocalReference iterType' iterName'
        newIterInstr = case direction of
          A.Up -> [ iterName' L.:= L.Add True True iter step' [] ]
          A.Down -> [ iterName' L.:= L.Sub True True iter step' [] ]
        preInstrs = case direction of
            A.Up ->
              [ cond L.:= L.ICmp LI.SLT iter to' [] ]
            A.Down ->
              [ cond L.:= L.ICmp LI.SGT iter to' [] ]
        branchTo l = case body'' of
          [] -> error "empty body of for-loop"
          (L.BasicBlock bodyLabel _ _:_) -> L.Do (L.CondBr (L.LocalReference (L.IntegerType 1) cond) bodyLabel l [])
        retTerm = L.Do (L.Br (L.Name "nextblock") [])
        true = L.ConstantOperand $ L.Int 1 1
        initIter = iterName' L.:= L.Select true from' from' []

        (pre,post) =
                ([L.BasicBlock label' [initIter] (L.Do (L.Br labelHead [])), L.BasicBlock labelHead preInstrs (branchTo labelEnd)]
                ,[L.BasicBlock labelEnd [] retTerm])
        body'' = body' ++ [L.BasicBlock labelLast newIterInstr (L.Do (L.Br labelHead []))]

    return (pre ++ body'' ++ post)
  ||]
qqLabeledInstructionE (A.ITE label cond then_body else_body) =
  [||do
    label' <- $$(qqExpM label)
    cond' <- $$(qqExpM cond)
    then_body' <- $$(qqExpM then_body)
    else_body' <- $$(qqExpM else_body)
    let labelString = case label' of
          L.Name n -> n
          L.UnName n -> fromString (show n)
        thenLabel = L.Name (labelString <> ".then")
        thenLastLabel = L.Name (labelString <> ".then.last")
        elseLabel = L.Name (labelString <> ".else")
        elseLastLabel = L.Name (labelString <> ".else.last")
        endLabel = L.Name (labelString <> ".end")
        headLabel = L.Name (labelString <> ".head")

        brEnd l = [L.BasicBlock l [] (L.Do (L.Br endLabel []))]
        pre = [L.BasicBlock label' [] (L.Do (L.Br headLabel []))
              ,L.BasicBlock headLabel [] (L.Do (L.CondBr cond' thenLabel elseLabel []))]
        brNext l = [L.BasicBlock l [] (L.Do (L.Br (L.Name "nextblock") []))]
        end = brNext endLabel
        then_body'' = brNext thenLabel ++ then_body' ++ brEnd thenLastLabel
        else_body'' = brNext elseLabel ++ else_body' ++ brEnd elseLastLabel
    return (pre ++ then_body'' ++ else_body'' ++ end)
  ||]
qqLabeledInstructionE (A.While label cond body) =
  [||do
    label' <- $$(qqExpM label)
    cond' <- $$(qqExpM cond)
    body' <- $$(qqExpM body)
    let labelString = case label' of
          L.Name n -> n
          L.UnName n -> fromString (show n)
        bodyLabel = L.Name (labelString <> ".body")
        bodyLastLabel = L.Name (labelString <> ".body.last")
        endLabel = L.Name (labelString <> ".end")
        headLabel = L.Name (labelString <> ".head")

        pre = [L.BasicBlock label' [] (L.Do (L.Br headLabel []))
              ,L.BasicBlock headLabel [] (L.Do (L.CondBr cond' bodyLabel endLabel []))]
        brNext l = [L.BasicBlock l [] (L.Do (L.Br (L.Name "nextblock") []))]
        end = brNext endLabel
        brTop = [L.BasicBlock bodyLastLabel [] (L.Do (L.Br headLabel []))]
        body'' = brNext bodyLabel ++ body' ++ brTop
    return (pre ++ body'' ++ end)
  ||]

qqNamedInstructionE :: Conversion A.NamedInstruction [L.BasicBlock]
qqNamedInstructionE (x1 A.:= x2) =
  [||do x1' <- $$(qqExpM x1)
        x2' <- $$(qqExpM x2)
        n <- newVariable
        case x2' of
          Left ins -> return [L.BasicBlock n [x1' L.:= ins] (L.Do $ L.Br (L.Name "nextblock") [])]
          Right term -> return [L.BasicBlock n [] (x1' L.:= term)]||]
qqNamedInstructionE (A.Do x2) =
  [||do x2' <- $$(qqExpM x2)
        n <- newVariable
        case x2' of
          Left ins -> return [L.BasicBlock n [L.Do ins] (L.Do $ L.Br (L.Name "nextblock") [])]
          Right term -> return [L.BasicBlock n [] (L.Do term)]||]
qqNamedInstructionE (A.AntiInstructionList s) =
  unsafeTExpCoerce $ antiVarE s
qqNamedInstructionE (A.AntiBasicBlock v)
  = [||(:[]) <$> $$(unsafeTExpCoerce $ antiVarE v)||]
qqNamedInstructionE (A.AntiBasicBlockList v)
  = unsafeTExpCoerce $ [|toBasicBlockList $(antiVarE v)|]

qqMetadataNodeIDE :: Conversion A.MetadataNodeID L.MetadataNodeID
qqMetadataNodeIDE (A.MetadataNodeID x1) =
  [||L.MetadataNodeID <$> $$(qqExpM x1)||]

qqMetadataNodeE :: Conversion A.MetadataNode L.MetadataNode
qqMetadataNodeE (A.MetadataNode x1) =
  [||L.MetadataNode <$> $$(qqExpM x1)||]
qqMetadataNodeE (A.MetadataNodeReference x1) =
  [||L.MetadataNodeReference <$> pure x1||]

qqMetadata :: Conversion A.Metadata L.Metadata
qqMetadata (A.MDString s) =
  [||L.MDString <$> $$(qqExpM s)||]
qqMetadata (A.MDValue v) =
  [||L.MDValue <$> $$(qqExpM v)||]
qqMetadata (A.MDNode n) =
  [||L.MDNode <$> $$(qqExpM n)||]

qqOperandE :: Conversion A.Operand L.Operand
qqOperandE (A.LocalReference x1 x2) =
  [||L.LocalReference <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqOperandE (A.ConstantOperand x1) =
  [||L.ConstantOperand <$> $$(qqExpM x1)||]
qqOperandE (A.MetadataOperand x1) =
  [||L.MetadataOperand <$> $$(qqExpM x1)||]
qqOperandE (A.AntiOperand s) =
  [||$$(unsafeTExpCoerce $ antiVarE s)||]

qqConstantE :: Conversion A.Constant L.Constant
qqConstantE (A.Int x1 x2) =
  [||L.Int <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqConstantE (A.IntAntiBs x1 x2) =
  [||let typeBits (L.IntegerType bs) = return bs
         typeBits t                  = fail $ "unexpected type: " ++ show t
     in L.Int <$> ($$(unsafeTExpCoerce (antiVarE x1)) >>= typeBits) <*> $$(qqExpM x2)||]
qqConstantE (A.Float x1) =
  [||L.Float <$> $$(qqExpM x1)||]
qqConstantE (A.Null x1) =
  [||L.Null <$> $$(qqExpM x1)||]
qqConstantE (A.Struct x1 x2 x3) =
  [||L.Struct <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqConstantE (A.Array x1 x2) =
  [||L.Array <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqConstantE (A.Vector x1) =
  [||L.Vector <$> $$(qqExpM x1)||]
qqConstantE (A.Undef x1) =
  [||L.Undef <$> $$(qqExpM x1)||]
qqConstantE (A.BlockAddress x1 x2) =
  [||L.BlockAddress <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqConstantE (A.GlobalReference x1 x2) =
  [||L.GlobalReference <$> $$(qqExpM x1)<*> $$(qqExpM x2)||]
qqConstantE (A.AntiConstant s) =
  unsafeTExpCoerce [|$(antiVarE s) >>= (return . toConstant)|]

qqNameE :: Conversion A.Name L.Name
qqNameE (A.Name x1) =
  [||L.Name <$> $$(qqExpM x1)||]
qqNameE (A.UnName x1) =
  [||L.UnName <$> $$(qqExpM x1)||]
qqNameE A.NeedsName = do
  n <- runIO $ atomicModifyIORef' counter $ \n -> (n+1,n)
  [||pure $ L.Name $ fromString $ "n" <> show (n :: Int)||]
qqNameE (A.AntiName s) =
  unsafeTExpCoerce [|$(antiVarE s) >>= return . toName|]

qqTypeE :: Conversion A.Type L.Type
qqTypeE A.VoidType =
  [||pure L.VoidType||]
qqTypeE (A.IntegerType x1) =
  [||L.IntegerType <$> $$(qqExpM x1)||]
qqTypeE (A.PointerType x1 x2) =
  [||L.PointerType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.FloatingPointType x1) =
  [||L.FloatingPointType <$> $$(qqExpM x1)||]
qqTypeE (A.FunctionType x1 x2 x3) =
  [||L.FunctionType <$> $$(qqExpM x1) <*> $$(qqExpM x2) <*> $$(qqExpM x3)||]
qqTypeE (A.VectorType x1 x2) =
  [||L.VectorType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.StructureType x1 x2) =
  [||L.StructureType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.ArrayType x1 x2) =
  [||L.ArrayType <$> $$(qqExpM x1) <*> $$(qqExpM x2)||]
qqTypeE (A.NamedTypeReference x1) =
  [||L.NamedTypeReference <$> $$(qqExpM x1)||]
qqTypeE A.MetadataType =
  [||pure L.MetadataType||]
qqTypeE (A.AntiType s) =
  [||$$(unsafeTExpCoerce $ antiVarE s)||]

qqDataLayoutE :: Conversion A.DataLayout L.DataLayout
qqDataLayoutE (A.DataLayout dl) =
  [||$$(qqExpM dl)||]
qqDataLayoutE (A.AntiDataLayout s) =
  unsafeTExpCoerce $ antiVarE s

qqTargetTripleE :: Conversion A.TargetTriple (Maybe ShortByteString)
qqTargetTripleE A.NoTargetTriple =
  [||pure Nothing||]
qqTargetTripleE (A.TargetTriple v) =
  [||Just <$> $$(qqExpM v)||]
qqTargetTripleE (A.AntiTargetTriple v) =
  unsafeTExpCoerce [|$(antiVarE v) >>= return . toTargetTriple|]

parse :: [A.Extensions]
      -> P.P a
      -> String
      -> Q a
parse exts p s = do
    loc <- location
    case P.parse (A.Antiquotation : exts) p (B.pack s) (locToPos loc) of
      Left err -> fail (show err)
      Right x  -> return x
  where
    locToPos :: Language.Haskell.TH.Loc -> Pos
    locToPos loc = Pos (loc_filename loc)
                       ((fst . loc_start) loc)
                       ((snd . loc_start) loc)
                       0

newtype TQuasiQuoter a = TQuasiQuoter { unTQuasiQuoter :: QuasiQuoter }

quasiquote :: forall a b. (Data a, QQExp a b)
           => [A.Extensions]
           -> P.P a
           -> TQuasiQuoter b
quasiquote exts p = TQuasiQuoter $
  QuasiQuoter { quoteExp  = parse exts p >=> unTypeQ . (qqExp :: a -> TExpQ b)
              , quotePat  = fail "LLVM pattern quasiquoter undefined"
              , quoteType = fail "LLVM type quasiquoter undefined"
              , quoteDec  = fail "LLVM declaration quasiquoter undefined"
              }

quasiquoteM :: forall a b m. (Data a, QQExp a b, CodeGenMonad m)
           => [A.Extensions]
           -> P.P a
           -> TQuasiQuoter (m b)
quasiquoteM exts p = TQuasiQuoter $
  QuasiQuoter { quoteExp  = parse exts p >=> unTypeQ . (qqExpM :: Conversion' m a b)
              , quotePat  = fail "LLVM monadic pattern quasiquoter undefined"
              , quoteType = fail "LLVM type quasiquoter undefined"
              , quoteDec  = fail "LLVM declaration quasiquoter undefined"
              }
