{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LLVM.Quote.Test.InlineAssembly where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Quote.LLVM
import LLVM.Quote.LLVM

import LLVM.AST
import LLVM.AST
import LLVM.AST.InlineAssembly as IA
import LLVM.AST.InlineAssembly as IA
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Global as G

tests = testGroup "InlineAssembly" [
  testCase "expression" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
                GlobalDefinition $
                  functionDefaults {
                    G.returnType = IntegerType 32,
                    G.name = Name "foo",
                    G.parameters = ([Parameter (IntegerType 32) (Name "x") []],False),
                    G.basicBlocks = [
                      BasicBlock (Name "entry") [
                        UnName 0 := Call {
                          tailCallKind = Nothing,
                          callingConvention = CC.C,
                          returnAttributes = [],
                          function = Left $ InlineAssembly {
                                       IA.type' = FunctionType (IntegerType 32) [IntegerType 32] False,
                                       assembly = "bswap $0",
                                       constraints = "=r,r",
                                       hasSideEffects = False,
                                       alignStack = False,
                                       dialect = ATTDialect
                                     },
                          arguments = [
                            (LocalReference (IntegerType 32) (Name "x"), [])
                           ],
                          functionAttributes = [],
                          metadata = []
                        }
                      ] (
                        Do $ Ret (Just (LocalReference (IntegerType 32) (UnName 0))) []
                      )
                    ]
                }

              ]
        s = [llmod|; ModuleID = '<string>'

             define i32 @foo(i32 %x) {
             entry:
               %0 = call i32 asm "bswap $0", "=r,r"(i32 %x)
               ret i32 %0
             }|]
    s @?= ast,

  testCase "module" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
                ModuleInlineAssembly "foo",
                ModuleInlineAssembly "bar",
                GlobalDefinition $ globalVariableDefaults {
                  G.name = UnName 0,
                  G.type' = IntegerType 32
                }
              ]
        s = [llmod|; ModuleID = '<string>'

             module asm "foo"
             module asm "bar"

             @0 = external global i32|]
    s @?= ast
 ]
