{-# LANGUAGE  QuasiQuotes #-}
module LLVM.General.Quote.Test.Loops where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.General.Quote.LLVM

import LLVM.General.AST
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IPred

tests :: TestTree
tests = testGroup "Loops" [
  testCase "nested for" $
    let globQ =
         [llg|define i32 @dotp(i32 %x) {
              entry:
              br label %bla

              bla:
              for i32 %i in 1 to %x with i32 0 as %n {
                body:
                for i32 %j in 1 to %x with i32 %n as %m {
                  blub:
                  %k = add i32 %m, 1
                  ret i32 %k
                }
                test:
                ret i32 %m
              }

              end:
              ret i32 %n
              }|]
        glob = G.Function {
                  G.linkage = L.External,
                  G.visibility = V.Default,
                  G.callingConvention = CC.C,
                  G.returnAttributes = [],
                  G.returnType = IntegerType {typeBits = 32},
                  G.name = Name "dotp",
                  G.parameters = ([G.Parameter IntegerType { typeBits = 32 } (Name "x") []], False),
                  G.functionAttributes = [],
                  G.section = Nothing,
                  G.alignment = 0,
                  G.garbageCollectorName = Nothing,
                  G.basicBlocks = [
                    G.BasicBlock (Name "entry") [] (Do Br { dest = Name "bla", metadata' = [] }),
                    G.BasicBlock (Name "bla") [
                      Name "i" := Phi {
                        type' = IntegerType {typeBits = 32},
                        incomingValues = [
                          ((LocalReference (Name "i.new")), (Name "test")),
                          (
                            (ConstantOperand C.Int { C.integerBits = 32, C.integerValue = 1 }),
                            (Name "entry")
                          )
                        ],
                        metadata = []
                      },
                      Name "n" := Phi {
                        type' = IntegerType {typeBits = 32},
                        incomingValues = [
                          ((LocalReference (Name "m")), (Name "test")),
                          (
                            (ConstantOperand C.Int { C.integerBits = 32, C.integerValue = 0 }),
                            (Name "entry")
                          )
                        ],
                        metadata = []
                      },
                      Name "bla.cond" := ICmp {
                        iPredicate = IPred.ULE,
                        operand0 = LocalReference (Name "i"),
                        operand1 = LocalReference (Name "x"),
                        metadata = []
                      },
                      Name "i.new" := Add {
                        nsw = True,
                        nuw = True,
                        operand0 = LocalReference (Name "i"),
                        operand1 = ConstantOperand C.Int {C.integerBits = 32, C.integerValue = 1},
                        metadata = []
                      }
                    ] (
                      Do CondBr {
                        condition = LocalReference (Name "bla.cond"),
                        trueDest = Name "body",
                        falseDest = Name "end",
                        metadata' = []
                      }
                    ),
                    G.BasicBlock (Name "body") [
                      Name "j" := Phi {
                        type' = IntegerType {typeBits = 32},
                        incomingValues = [
                          ((LocalReference (Name "j.new")), (Name "blub")),
                          (
                            (ConstantOperand C.Int { C.integerBits = 32, C.integerValue = 1 }),
                            (Name "bla")
                          )
                        ],
                        metadata = []
                      },
                      Name "m" := Phi {
                        type' = IntegerType {typeBits = 32},
                        incomingValues = [
                          ((LocalReference (Name "k")), (Name "blub")),
                          ((LocalReference (Name "n")), (Name "bla"))
                        ],
                        metadata = []
                      },
                      Name "body.cond" := ICmp {
                        iPredicate = IPred.ULE,
                        operand0 = LocalReference (Name "j"),
                        operand1 = LocalReference (Name "x"),
                        metadata = []
                      },
                      Name "j.new" := Add {
                        nsw = True,
                        nuw = True,
                        operand0 = LocalReference (Name "j"),
                        operand1 = ConstantOperand C.Int {C.integerBits = 32, C.integerValue = 1},
                        metadata = []
                      }
                    ] (
                      Do CondBr {
                        condition = LocalReference (Name "body.cond"),
                        trueDest = Name "blub",
                        falseDest = Name "test",
                        metadata' = []
                      }
                    ),
                    G.BasicBlock (Name "blub") [
                      Name "k" := Add {
                        nsw = False,
                        nuw = False,
                        operand0 = LocalReference (Name "m"),
                        operand1 = ConstantOperand C.Int {C.integerBits = 32, C.integerValue = 1},
                        metadata = []
                      }
                    ] (Do Br { dest = Name "body", metadata' = [] }),
                    G.BasicBlock (Name "test") [] (Do Br { dest = Name "bla", metadata' = [] }),
                    G.BasicBlock (Name "end") [] (
                      Do Ret {returnOperand = Just (LocalReference (Name "n")), metadata' = []}
                    )
                  ]
                }
    in globQ @?= glob
  ]
