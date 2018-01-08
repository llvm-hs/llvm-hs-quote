{-# LANGUAGE  QuasiQuotes #-}
module LLVM.Quote.Test.Constants where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Data.Functor
import Data.Maybe
import Foreign.Ptr
import Data.Word

import LLVM.Quote.LLVM
import LLVM.Quote.LLVM

import LLVM.AST
import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Type
import LLVM.AST.Name
import LLVM.AST.Name
import LLVM.AST.AddrSpace
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.IntegerPredicate as IPred

tests = testGroup "Constants" [
  testCase name $ mASTQ @?= mAST
  | (name, type', value, mASTQ) <- [
    (
      "integer",
      IntegerType 32,
      C.Int 32 1,
      [lldef|@0 = global i32 1|]
    ), (
      "wide integer",
      IntegerType 65,
      C.Int 65 1,
      [lldef|@0 = global i65 1|]
    ), (
      "big wide integer",
      IntegerType 66,
      C.Int 66 20000000000000000000,
      [lldef|@0 = global i66 20000000000000000000|]
    ), (
      "negative wide integer",
      IntegerType 65,
      C.Int 65 (-1),
      [lldef|@0 = global i65 -1|]
    ), (
      "float",
      FloatingPointType FloatFP,
      C.Float (F.Single 1),
      [lldef|@0 = global float 1.000000e+00|]
    ), (
      "double",
      FloatingPointType DoubleFP,
      C.Float (F.Double 1),
      [lldef|@0 = global double 1.000000e+00|]
    ), (
      "struct",
      StructureType False (replicate 2 (IntegerType 32)),
      C.Struct Nothing False (replicate 2 (C.Int 32 1)),
      [lldef|@0 = global { i32, i32 } { i32 1, i32 1 }|]
    ), (
      "dataarray",
      ArrayType 3 (IntegerType 32),
      C.Array (IntegerType 32) [C.Int 32 i | i <- [1,2,1]],
      [lldef|@0 = global [3 x i32] [i32 1, i32 2, i32 1]|]
    ), (
      "array",
      ArrayType 3 (StructureType False [IntegerType 32]),
      C.Array (StructureType False [IntegerType 32]) [C.Struct Nothing False [C.Int 32 i] | i <- [1,2,1]],
      [lldef|@0 = global [3 x { i32 }] [{ i32 } { i32 1 }, { i32 } { i32 2 }, { i32 } { i32 1 }]|]
    ), (
      "datavector",
      VectorType 3 (IntegerType 32),
      C.Vector [C.Int 32 i | i <- [1,2,1]],
      [lldef|@0 = global <3 x i32> <i32 1, i32 2, i32 1>|]
    ), (
      "undef",
      IntegerType 32,
      C.Undef (IntegerType 32),
      [lldef|@0 = global i32 undef|]
    )
   ],
   let mAST = GlobalDefinition $ globalVariableDefaults {
                G.name = UnName 0, G.type' = type', G.initializer = Just value
             }
 ]
