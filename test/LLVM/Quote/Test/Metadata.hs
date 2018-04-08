{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LLVM.Quote.Test.Metadata where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Quote.LLVM

import LLVM.AST as A
import LLVM.AST.Type
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as G

tests :: TestTree
tests = testGroup "Metadata" [
  testCase "local" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ globalVariableDefaults { G.name = UnName 0, G.type' = IntegerType 32 },
          GlobalDefinition $ functionDefaults {
            G.returnType = IntegerType 32,
            G.name = Name "foo",
            G.basicBlocks = [
              BasicBlock (Name "entry") [
                 UnName 0 := Load {
                            volatile = False,
                            address = ConstantOperand (C.GlobalReference (ptr i32) (UnName 0)),
                            maybeAtomicity = Nothing,
                            A.alignment = 0,
                            metadata = []
                          }
                 ] (
                 Do $ Ret (Just (ConstantOperand (C.Int 32 0))) [
                   (
                     "my-metadatum",
                     MetadataNode [
                      Just $ MDString "super hyper"
                     ]
                   )
                 ]
               )
             ]
           }
         ]
    let s = [llmod|; ModuleID = '<string>'

            @0 = external global i32

            define i32 @foo() {
            entry:
              %0 = load i32, i32* @0
              ret i32 0, !my-metadatum !{!"super hyper"}
            }|]
    s @?= ast,

  testCase "global" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
            G.returnType = IntegerType 32,
            G.name = Name "foo",
            G.basicBlocks = [
              BasicBlock (Name "entry") [
              ] (
                Do $ Ret (Just (ConstantOperand (C.Int 32 0))) [
                  ("my-metadatum", MetadataNodeReference (MetadataNodeID 0))
                ]
              )
             ]
            },
          MetadataNodeDefinition (MetadataNodeID 0) [ Just . MDValue $ ConstantOperand (C.Int 32 1) ]
         ]
    let s = [llmod|; ModuleID = '<string>'

            define i32 @foo() {
            entry:
              ret i32 0, !my-metadatum !0
            }

            !0 = metadata !{i32 1}
            |]
    s @?= ast,

  testCase "named" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          NamedMetadataDefinition "my-module-metadata" [ MetadataNodeID 0 ],
          MetadataNodeDefinition (MetadataNodeID 0) [ Just . MDValue $ ConstantOperand (C.Int 32 1) ]
         ]
    let s = [llmod|; ModuleID = '<string>'

            !my-module-metadata = !{!0}

            !0 = metadata !{i32 1}|]
    s @?= ast,

  testCase "null" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          NamedMetadataDefinition "my-module-metadata" [ MetadataNodeID 0 ],
          MetadataNodeDefinition (MetadataNodeID 0) [ Nothing ]
         ]
    let s = [llmod|; ModuleID = '<string>'

            !my-module-metadata = !{!0}

            !0 = metadata !{null}|]
    s @?= ast,

  testGroup "cyclic" [
    testCase "metadata-only" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing [
            NamedMetadataDefinition "my-module-metadata" [MetadataNodeID 0],
            MetadataNodeDefinition (MetadataNodeID 0) [
              Just . MDNode $ MetadataNodeReference (MetadataNodeID 1)
             ],
            MetadataNodeDefinition (MetadataNodeID 1) [
              Just . MDNode $ MetadataNodeReference (MetadataNodeID 0)
             ]
           ]
      let s = [llmod|; ModuleID = '<string>'

              !my-module-metadata = !{!0}

              !0 = metadata !{!1}
              !1 = metadata !{!0}|]
      s @?= ast,

    testCase "metadata-global" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = VoidType,
              G.name = Name "foo",
              G.basicBlocks = [
                BasicBlock (Name "entry") [
                 ] (
                   Do $ Ret Nothing [ ("my-metadatum", MetadataNodeReference (MetadataNodeID 0)) ]
                 )
               ]
             },
            MetadataNodeDefinition (MetadataNodeID 0) [
              Just . MDValue $ ConstantOperand (C.GlobalReference (ptr (FunctionType void [] False)) (Name "foo"))
             ]
           ]
      let s = [llmod|; ModuleID = '<string>'

              define void @foo() {
              entry:
                ret void, !my-metadatum !0
              }

              !0 = metadata !{void ()* @foo}|]
      s @?= ast
   ]

 ]
