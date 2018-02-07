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
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.AST.Type

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
    s @?= ast
  ]
