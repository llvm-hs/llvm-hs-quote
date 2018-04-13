{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LLVM.Quote.Test.Module
  ( tests
  )where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Quote.LLVM

import LLVM.AST
import LLVM.AST.CallingConvention
import LLVM.AST.Constant hiding (GetElementPtr, Mul, Add)
import LLVM.AST.Global
import LLVM.AST.Type
import qualified LLVM.AST.ParameterAttribute as PA

import Data.Word

tests :: TestTree
tests = testGroup "Module" [
  testCase "calls have the proper type" $ do
    let s = [llmod|
             define void @f() {
             entry:
               call void @g()
               ret void
             }
             define void @g() {
             entry:
               ret void
             }
            |]
    let ast = Module "<string>" "<string>" Nothing Nothing
          [ GlobalDefinition functionDefaults
              { name = "f"
              , returnType = void
              , basicBlocks =
                  [ BasicBlock
                      "entry"
                      [Do
                         (Call
                            Nothing
                            C
                            []
                            (Right (ConstantOperand (GlobalReference (ptr (FunctionType void [] False)) (Name "g"))))
                            []
                            []
                            [])
                      ]
                      (Do (Ret Nothing []))
                  ]
              }
          , GlobalDefinition functionDefaults
              { name = "g"
              , returnType = void
              , basicBlocks =
                  [ BasicBlock "entry" [] (Do (Ret Nothing []))
                  ]
              }
          ]
    s @?= ast,
  testCase "saxpy matrix kernel" $ do
    -- https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms#Level_1
    let s = [llmod|
define void @saxpy(i32* noalias nocapture %x, i32* noalias nocapture %y, i32 %a, i64 %i) {
entry:
  %1 = getelementptr inbounds i32, i32* %x, i64 %i
  %2 = load i32, i32* %1, align 4
  %3 = mul nsw i32 %2, %a
  %4 = getelementptr inbounds i32, i32* %y, i64 %i
  %5 = load i32, i32* %4, align 4
  %6 = add nsw i32 %3, %5
  store i32 %6, i32* %1, align 4
  %7 = add i64 %i, 1
  %8 = getelementptr inbounds i32, i32* %x, i64 %7
  %9 = load i32, i32* %8, align 4
  %10 = mul nsw i32 %9, %a
  %11 = getelementptr inbounds i32, i32* %y, i64 %7
  %12 = load i32, i32* %11, align 4
  %13 = add nsw i32 %10, %12
  store i32 %13, i32* %8, align 4
  %14 = add i64 %i, 2
  %15 = getelementptr inbounds i32, i32* %x, i64 %14
  %16 = load i32, i32* %15, align 4
  %17 = mul nsw i32 %16, %a
  %18 = getelementptr inbounds i32, i32* %y, i64 %14
  %19 = load i32, i32* %18, align 4
  %20 = add nsw i32 %17, %19
  store i32 %20, i32* %15, align 4
  %21 = add i64 %i, 3
  %22 = getelementptr inbounds i32, i32* %x, i64 %21
  %23 = load i32, i32* %22, align 4
  %24 = mul nsw i32 %23, %a
  %25 = getelementptr inbounds i32, i32* %y, i64 %21
  %26 = load i32, i32* %25, align 4
  %27 = add nsw i32 %24, %26
  store i32 %27, i32* %22, align 4
  ret void
}
|]
        ast = Module "<string>" "<string>" Nothing Nothing
          [ GlobalDefinition functionDefaults
              { name = "saxpy"
              , parameters = ([Parameter (ptr i32) "x" [PA.NoAlias, PA.NoCapture], Parameter (ptr i32) "y" [PA.NoAlias, PA.NoCapture], Parameter i32 "a" [], Parameter i64 "i" []], False)
              , returnType = void
              , basicBlocks =
                  [ BasicBlock
                      "entry"
                      [ UnName 1 := gep (ref (ptr i32) "x") [ref i64 "i"]
                      , UnName 2 := load (ref (ptr i32) (UnName 1))
                      , UnName 3 := mul (ref i32 (UnName 2)) (ref i32 "a")
                      , UnName 4 := gep (ref (ptr i32) "y") [ref i64 "i"]
                      , UnName 5 := load (ref (ptr i32) (UnName 4))
                      , UnName 6 := add (ref i32 (UnName 3)) (ref i32 (UnName 5))
                      , Do (store (ref i32 (UnName 6)) (ref (ptr i32) (UnName 1)))
                      , UnName 7 := Add False False (ref i64 "i") (cint 64 1) []
                      , UnName 8 := gep (ref (ptr i32) "x") [ref i64 (UnName 7)]
                      , UnName 9 := load (ref (ptr i32) (UnName 8))
                      , UnName 10 := mul (ref i32 (UnName 9)) (ref i32 "a")
                      , UnName 11 := gep (ref (ptr i32) "y") [ref i64 (UnName 7)]
                      , UnName 12 := load (ref (ptr i32) (UnName 11))
                      , UnName 13 := add (ref i32 (UnName 10)) (ref i32 (UnName 12))
                      , Do (store (ref i32 (UnName 13)) (ref (ptr i32) (UnName 8)))
                      , UnName 14 := Add False False (ref i64 "i") (cint 64 2) []
                      , UnName 15 := gep (ref (ptr i32) "x") [ref i64 (UnName 14)]
                      , UnName 16 := load (ref (ptr i32) (UnName 15))
                      , UnName 17 := mul (ref i32 (UnName 16)) (ref i32 "a")
                      , UnName 18 := gep (ref (ptr i32) "y") [ref i64 (UnName 14)]
                      , UnName 19 := load (ref (ptr i32) (UnName 18))
                      , UnName 20 := add (ref i32 (UnName 17)) (ref i32 (UnName 19))
                      , Do (store (ref i32 (UnName 20)) (ref (ptr i32) (UnName 15)))
                      , UnName 21 := Add False False (ref i64 "i") (cint 64 3) []
                      , UnName 22 := gep (ref (ptr i32) "x") [ref i64 (UnName 21)]
                      , UnName 23 := load (ref (ptr i32) (UnName 22))
                      , UnName 24 := mul (ref i32 (UnName 23)) (ref i32 "a")
                      , UnName 25 := gep (ref (ptr i32) "y") [ref i64 (UnName 21)]
                      , UnName 26 := load (ref (ptr i32) (UnName 25))
                      , UnName 27 := add (ref i32 (UnName 24)) (ref i32 (UnName 26))
                      , Do (store (ref i32 (UnName 27)) (ref (ptr i32) (UnName 22)))
                      ]
                      (Do (Ret Nothing []))
                  ]
              }
          ]
    s @?= ast
  ]

cint :: Word32 -> Integer -> Operand
cint w v = ConstantOperand (Int w v)

ref :: Type -> Name -> Operand
ref = LocalReference

gep :: Operand -> [Operand] -> Instruction
gep a is = GetElementPtr True a is []

load :: Operand -> Instruction
load a = Load False a Nothing 4 []

store :: Operand -> Operand -> Instruction
store v a = Store False a v Nothing 4 []

mul :: Operand -> Operand -> Instruction
mul x y = Mul True False x y []

add :: Operand -> Operand -> Instruction
add x y = Add True False x y []
