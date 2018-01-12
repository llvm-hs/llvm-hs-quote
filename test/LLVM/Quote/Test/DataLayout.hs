{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LLVM.Quote.Test.DataLayout where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe
import Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.Quote.LLVM

import LLVM.AST
import LLVM.DataLayout
import LLVM.AST.DataLayout
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Global as G

m, t :: [Char] -> [Char]
m s = "; ModuleID = '<string>'\n" ++ s
t s = "target datalayout = \"" ++ s ++ "\"\n"

tests :: TestTree
tests = testGroup "DataLayout" [
  testCase name $ sdl @?= (Module "<string>" "<string>" mdl Nothing [])
  | (name, mdl, sdl) <- [
    ("none", Nothing, [llmod||]),

    ("little-endian", Just (defaultDataLayout LittleEndian) { endianness = LittleEndian }, [llmod|target datalayout = "e"|]),

    ("big-endian", Just (defaultDataLayout LittleEndian) { endianness = BigEndian }, [llmod|target datalayout = "E"|]),

    ("native", Just (defaultDataLayout BigEndian) { nativeSizes = Just (Set.fromList [8,32]) }, [llmod|target datalayout = "n8:32"|]),

    (
     "no pref",
     Just $ (defaultDataLayout BigEndian) {
       pointerLayouts =
         Map.singleton
         (AddrSpace 0)
         (
          8,
          AlignmentInfo {
            abiAlignment = 64,
            preferredAlignment = 64
          }
         )
     },
     [llmod|target datalayout = "p:8:64"|]

    ), (
     "addrSpace",
     Just (DataLayout {endianness = BigEndian, mangling = Nothing, stackAlignment = Nothing,
      pointerLayouts = Map.fromList [
        (AddrSpace 0,(64,AlignmentInfo {abiAlignment = 64, preferredAlignment = 64}))
        ,(AddrSpace 1,(8,AlignmentInfo {abiAlignment = 64, preferredAlignment = 64}))
        ]
        , typeLayouts = Map.fromList [
          ((IntegerAlign,1),AlignmentInfo {abiAlignment = 8, preferredAlignment = 8}),
          ((IntegerAlign,8),AlignmentInfo {abiAlignment = 8, preferredAlignment = 8}),
          ((IntegerAlign,16),AlignmentInfo {abiAlignment = 16, preferredAlignment = 16}),
          ((IntegerAlign,32),AlignmentInfo {abiAlignment = 32, preferredAlignment = 32}),
          ((IntegerAlign,64),AlignmentInfo {abiAlignment = 32, preferredAlignment = 64}),
          ((VectorAlign,64),AlignmentInfo {abiAlignment = 64, preferredAlignment = 64}),
          ((VectorAlign,128),AlignmentInfo {abiAlignment = 128, preferredAlignment = 128}),
          ((FloatAlign,16),AlignmentInfo {abiAlignment = 16, preferredAlignment = 16}),
          ((FloatAlign,32),AlignmentInfo {abiAlignment = 32, preferredAlignment = 32}),
          ((FloatAlign,64),AlignmentInfo {abiAlignment = 64, preferredAlignment = 64}),
          ((FloatAlign,128),AlignmentInfo {abiAlignment = 128, preferredAlignment = 128})]
        , aggregateLayout = AlignmentInfo {abiAlignment = 0, preferredAlignment = 64}
        , nativeSizes = Nothing}
        )
     ,
     [llmod|target datalayout = "p1:8:64"|]

    ), (
     "big",
      Just (DataLayout {endianness = LittleEndian, mangling = Nothing, stackAlignment = Just 128, pointerLayouts = Map.fromList
        [(AddrSpace 0,(64,AlignmentInfo {abiAlignment = 64, preferredAlignment = 64}))]
        , typeLayouts = Map.fromList [
            ((IntegerAlign,1),AlignmentInfo {abiAlignment = 8, preferredAlignment = 8}),
            ((IntegerAlign,8),AlignmentInfo {abiAlignment = 8, preferredAlignment = 8}),
            ((IntegerAlign,16),AlignmentInfo {abiAlignment = 16, preferredAlignment = 16}),
            ((IntegerAlign,32),AlignmentInfo {abiAlignment = 32, preferredAlignment = 32}),
            ((IntegerAlign,64),AlignmentInfo {abiAlignment = 64, preferredAlignment = 64}),
            ((VectorAlign,64),AlignmentInfo {abiAlignment = 64, preferredAlignment = 64}),
            ((VectorAlign,128),AlignmentInfo {abiAlignment = 128, preferredAlignment = 128}),
            ((FloatAlign,16),AlignmentInfo {abiAlignment = 16, preferredAlignment = 16}),
            ((FloatAlign,32),AlignmentInfo {abiAlignment = 32, preferredAlignment = 32}),
            ((FloatAlign,64),AlignmentInfo {abiAlignment = 64, preferredAlignment = 64}),
            ((FloatAlign,80),AlignmentInfo {abiAlignment = 128, preferredAlignment = 128}),
            ((FloatAlign,128),AlignmentInfo {abiAlignment = 128, preferredAlignment = 128})
            ]
        , aggregateLayout = AlignmentInfo {abiAlignment = 0, preferredAlignment = 64}
        , nativeSizes = Just (Set.fromList [8,16,32,64])}
        )
      ,
     [llmod|target datalayout = "e-i64:64-f80:128-n8:16:32:64-S128"|]
    )
   ]
 ]
