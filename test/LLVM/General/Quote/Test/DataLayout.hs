{-# LANGUAGE  QuasiQuotes #-}
module LLVM.General.Quote.Test.DataLayout where

import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.General.Quote.LLVM

import LLVM.General.AST
import LLVM.General.AST.DataLayout
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Global as G

m s = "; ModuleID = '<string>'\n" ++ s
t s = "target datalayout = \"" ++ s ++ "\"\n"

tests = testGroup "DataLayout" [
  testCase name $ sdl @?= (Module "<string>" mdl Nothing [])
  | (name, mdl, sdl) <- [
    ("none", Nothing, [llmod||]),
    ("little-endian", Just defaultDataLayout { endianness = Just LittleEndian }, [llmod|target datalayout = "e"|]),
    ("big-endian", Just defaultDataLayout { endianness = Just BigEndian }, [llmod|target datalayout = "E"|]),
    ("native", Just defaultDataLayout { nativeSizes = Just (Set.fromList [8,32]) }, [llmod|target datalayout = "n8:32"|]),
    (
     "no pref",
     Just defaultDataLayout {
       pointerLayouts =
         Map.singleton
         (AddrSpace 0)
         (
          8,
          AlignmentInfo {
            abiAlignment = 64,
            preferredAlignment = Nothing
          }
         )
     },
     [llmod|target datalayout = "p:8:64"|]
    ), (
     "no pref",
     Just defaultDataLayout {
       pointerLayouts =
         Map.singleton
         (AddrSpace 1)
         (
          8,
          AlignmentInfo {
            abiAlignment = 32,
            preferredAlignment = Just 64
          }
         )
     },
     [llmod|target datalayout = "p1:8:32:64"|]
    ), (
     "big",
     Just DataLayout {
       endianness = Just LittleEndian,
       stackAlignment = Just 128,
       pointerLayouts = Map.fromList [
         (AddrSpace 0, (64, AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64}))
        ],
       typeLayouts = Map.fromList [
         ((IntegerAlign, 1), AlignmentInfo {abiAlignment = 8, preferredAlignment = Just 8}),
         ((IntegerAlign, 8), AlignmentInfo {abiAlignment = 8, preferredAlignment = Just 8}),
         ((IntegerAlign, 16), AlignmentInfo {abiAlignment = 16, preferredAlignment = Just 16}),
         ((IntegerAlign, 32), AlignmentInfo {abiAlignment = 32, preferredAlignment = Just 32}),
         ((IntegerAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64}),
         ((VectorAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64}),
         ((VectorAlign, 128), AlignmentInfo {abiAlignment = 128, preferredAlignment = Just 128}),
         ((FloatAlign, 32), AlignmentInfo {abiAlignment = 32, preferredAlignment = Just 32}),
         ((FloatAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64}),
         ((FloatAlign, 80), AlignmentInfo {abiAlignment = 128, preferredAlignment = Just 128}),
         ((AggregateAlign, 0), AlignmentInfo {abiAlignment = 0, preferredAlignment = Just 64}),
         ((StackAlign, 0), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64})
        ],
       nativeSizes = Just (Set.fromList [8,16,32,64])
     },
     [llmod|target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"|]
    )
   ]
 ]