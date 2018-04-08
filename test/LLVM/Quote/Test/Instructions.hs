{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LLVM.Quote.Test.Instructions where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Quote.LLVM

import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.Type
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPPred
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.RMWOperation as RMWOp
import qualified LLVM.AST.Float as Float

instruction :: Type -> Operand -> Instruction
instruction ty op = [lli| call void @dummy_fuc($type:ty $opr:op) |]

terminator :: Terminator
terminator = [llt| ret i32 0 |]

brWithName :: Name -> Name -> Name -> Instruction
brWithName condName n1 n2 = [lli|br i1 $id:condName, label $id:n1, label $id:n2|]

brWithOp :: Operand -> Name -> Name -> Instruction
brWithOp cond n1 n2 = [lli|br $opr:cond, label $id:n1, label $id:n2|]

phi1 :: Type -> Name -> Name -> Name -> Name -> Instruction
phi1 ty vName1 vName2 label1 label2 = [lli|phi $type:ty [$id:vName1, $id:label1], [$id:vName2, $id:label2]|]

phi2 :: Type -> Operand -> Operand -> Name -> Name -> Instruction
phi2 ty op1 op2 label1 label2 = [lli|phi $type:ty [$opr:op1, $id:label1], [$opr:op2, $id:label2]|]


retWithName :: Type -> Name -> Terminator
retWithName ty name = [llt|ret $type:ty $id:name|]

retWithOp :: Type -> Operand -> Terminator
retWithOp ty op = [llt|ret $type:ty $opr:op|]

-- `declare` with function attribute
def1 :: Definition
def1 = [lldef|declare void @llvm.gcroot(i8**, i8*) nounwind|]

tests :: TestTree
tests = let a t = LocalReference t . UnName in testGroup "Instructions" [
  testGroup "regular" [
    testCase name $ instrQ @?= instr
    | (name, instr, instrQ) <- [
          ("add",
           Add {
             nsw = False,
             nuw = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|add i32 %0, %0|]),
          ("nsw",
           Add {
             nsw = True,
             nuw = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|add nsw i32 %0, %0|]),
          ("nuw",
           Add {
             nsw = False,
             nuw = True,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|add nuw i32 %0, %0|]),
          ("fadd",
           FAdd {
             fastMathFlags = NoFastMathFlags,
             operand0 = a float 1,
             operand1 = a float 1,
             metadata = []
           },
           [lli|fadd float %1, %1|]),
          ("sub",
           Sub {
             nsw = False,
             nuw = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|sub i32 %0, %0|]),
          ("fsub",
           FSub {
             fastMathFlags = NoFastMathFlags,
             operand0 = a float 1,
             operand1 = a float 1,
             metadata = []
           },
           [lli|fsub float %1, %1|]),
          ("mul",
           Mul {
             nsw = False,
             nuw = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|mul i32 %0, %0|]),
          ("fmul",
           FMul {
             fastMathFlags = NoFastMathFlags,
             operand0 = a float 1,
             operand1 = a float 1,
             metadata = []
           },
           [lli|fmul float %1, %1|]),
          ("udiv",
           UDiv {
             exact = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|udiv i32 %0, %0|]),
          ("exact",
           UDiv {
             exact = True,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|udiv exact i32 %0, %0|]),
          ("sdiv",
           SDiv {
             exact = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|sdiv i32 %0, %0|]),
          ("fdiv",
           FDiv {
             fastMathFlags = NoFastMathFlags,
             operand0 = a float 1,
             operand1 = a float 1,
             metadata = []
           },
           [lli|fdiv float %1, %1|]),
          ("urem",
           URem {
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|urem i32 %0, %0|]),
          ("srem",
           SRem {
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|srem i32 %0, %0|]),
          ("frem",
           FRem {
             fastMathFlags = NoFastMathFlags,
             operand0 = a float 1,
             operand1 = a float 1,
             metadata = []
           },
           [lli|frem float %1, %1|]),
          ("shl",
           Shl {
             nsw = False,
             nuw = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|shl i32 %0, %0|]),
          ("ashr",
           AShr {
             exact = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|ashr i32 %0, %0|]),
          ("lshr",
           LShr {
             exact = False,
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|lshr i32 %0, %0|]),
          ("and",
           And {
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|and i32 %0, %0|]),
          ("or",
           Or {
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|or i32 %0, %0|]),
          ("xor",
           Xor {
             operand0 = a i32 0,
             operand1 = a i32 0,
             metadata = []
           },
           [lli|xor i32 %0, %0|]),
          ("alloca",
           Alloca {
             allocatedType = IntegerType 32,
             numElements = Nothing,
             alignment = 0,
             metadata = []
           },
           [lli|alloca i32|]),
          ("alloca tricky",
           Alloca {
             allocatedType = IntegerType 7,
             numElements = Just (ConstantOperand (C.Int 32 2)),
             alignment = 128,
             metadata = []
           },
           [lli|alloca i7, i32 2, align 128|]),
          ("load",
           Load {
             volatile = False,
             address = a (ptr i32) 2,
             maybeAtomicity = Nothing,
             alignment = 0,
             metadata = []
           },
           [lli|load i32* %2|]),
          ("volatile",
           Load {
             volatile = True,
             address = a (ptr i32) 2,
             maybeAtomicity = Nothing,
             alignment = 0,
             metadata = []
           },
           [lli|load volatile i32* %2|]),
          ("acquire",
           Load {
             volatile = False,
             address = a (ptr i32) 2,
             maybeAtomicity = Just (System, Acquire),
             alignment = 1,
             metadata = []
           },
           [lli|load atomic i32* %2 acquire, align 1|]),
          ("singlethread",
           Load {
             volatile = False,
             address = a (ptr i32) 2,
             maybeAtomicity = Just (SingleThread, Monotonic),
             alignment = 1,
             metadata = []
           },
           [lli|load atomic i32* %2 singlethread monotonic, align 1|]),
          ("GEP",
           GetElementPtr {
             inBounds = False,
             address = a (ptr i32) 2,
             indices = [ a i32 0 ],
             metadata = []
           },
           [lli|getelementptr i32* %2, i32 %0|]),
          ("inBounds",
           GetElementPtr {
             inBounds = True,
             address = a (ptr i32) 2,
             indices = [ a i32 0 ],
             metadata = []
           },
           [lli|getelementptr inbounds i32* %2, i32 %0|]),
          ("cmpxchg",
           CmpXchg {
             volatile = False,
             address = a (ptr i32) 2,
             expected = a i32 0,
             replacement = a i32 0,
             atomicity = (System, Monotonic),
             metadata = [],
             failureMemoryOrdering = Unordered
           },
           [lli|cmpxchg i32* %2, i32 %0, i32 %0 monotonic|]),
          ("atomicrmw",
           AtomicRMW {
             volatile = False,
             rmwOperation = RMWOp.UMax,
             address = a (ptr i32) 2,
             value = a i32 0,
             atomicity = (System, Release),
             metadata = []
           },
           [lli|atomicrmw umax i32* %2, i32 %0 release|]),

          ("trunc",
           Trunc {
             operand0 = a i32 0,
             type' = IntegerType 16,
             metadata = []
           },
           [lli|trunc i32 %0 to i16|]),
          ("zext",
           ZExt {
             operand0 = a i32 0,
             type' = IntegerType 64,
             metadata = []
           },
           [lli|zext i32 %0 to i64|]),
          ("sext",
           SExt {
             operand0 = a i32 0,
             type' = IntegerType 64,
             metadata = []
           },
           [lli|sext i32 %0 to i64|]),
          ("fptoui",
           FPToUI {
             operand0 = a float 1,
             type' = IntegerType 64,
             metadata = []
           },
           [lli|fptoui float %1 to i64|]),
          ("fptosi",
           FPToSI {
             operand0 = a float 1,
             type' = IntegerType 64,
             metadata = []
           },
           [lli|fptosi float %1 to i64|]),
          ("uitofp",
           UIToFP {
             operand0 = a i32 0,
             type' = FloatingPointType FloatFP,
             metadata = []
           },
           [lli|uitofp i32 %0 to float|]),
          ("sitofp",
           SIToFP {
             operand0 = a i32 0,
             type' = FloatingPointType FloatFP,
             metadata = []
           },
           [lli|sitofp i32 %0 to float|]),
          ("fptrunc",
           FPTrunc {
             operand0 = a float 1,
             type' = FloatingPointType HalfFP,
             metadata = []
           },
           [lli|fptrunc float %1 to half|]),
          ("fpext",
           FPExt {
             operand0 = a float 1,
             type' = FloatingPointType DoubleFP,
             metadata = []
           },
           [lli|fpext float %1 to double|]),
          ("ptrtoint",
           PtrToInt {
             operand0 = a (ptr i32) 2,
             type' = IntegerType 32,
             metadata = []
           },
           [lli|ptrtoint i32* %2 to i32|]),
          ("inttoptr",
           IntToPtr {
             operand0 = a i32 0,
             type' = PointerType (IntegerType 32) (AddrSpace 0),
             metadata = []
           },
           [lli|inttoptr i32 %0 to i32*|]),
          ("bitcast",
           BitCast {
             operand0 = a i32 0,
             type' = FloatingPointType FloatFP,
             metadata = []
           },
           [lli|bitcast i32 %0 to float|]),
          ("addrspacecast",
           AddrSpaceCast {
             operand0 = a (ptr i32) 2,
             type' = PointerType (IntegerType 32) (AddrSpace 2),
             metadata = []
           },
           [lli|addrspacecast i32* %2 to i32 addrspace(2)*|]),
          ("select",
           Select {
             condition' = a i1 4,
             trueValue = a i32 0,
             falseValue = a i32 0,
             metadata = []
           },
           [lli|select i1 %4, i32 %0, i32 %0|]),
          ("vaarg",
           VAArg {
             argList = a (ptr i32) 2,
             type' = IntegerType 16,
             metadata = []
           },
           [lli|va_arg i32* %2, i16|]),
          ("extractelement",
           ExtractElement {
             vector = a (VectorType 2 i32) 5,
             index = a i32 0,
             metadata = []
           },
           [lli|extractelement <2 x i32> %5, i32 %0|]),
          ("insertelement",
           InsertElement {
             vector = a (VectorType 2 i32) 5,
             element = a i32 0,
             index = a i32 0,
             metadata = []
           },
           [lli|insertelement <2 x i32> %5, i32 %0, i32 %0|]),
          ("shufflevector",
           ShuffleVector {
             operand0 = a (VectorType 2 i32) 5,
             operand1 = a (VectorType 2 i32) 5,
             mask = C.Vector [ C.Int 32 p | p <- [0..1] ],
             metadata = []
           },
           [lli|shufflevector <2 x i32> %5, <2 x i32> %5, <2 x i32> <i32 0, i32 1>|]),
          ("extractvalue",
           ExtractValue {
             aggregate = a (StructureType False [i32, i32]) 6,
             indices' = [0],
             metadata = []
           },
           [lli|extractvalue { i32, i32 } %6, 0|]),
          ("insertvalue",
           InsertValue {
             aggregate = a (StructureType False [i32, i32]) 6,
             element = a i32 0,
             indices' = [0],
             metadata = []
           },
           [lli|insertvalue { i32, i32 } %6, i32 %0, 0|]),
          ("landingpad-catch",
           LandingPad {
             type' = StructureType False [
                PointerType (IntegerType 8) (AddrSpace 0),
                IntegerType 32
               ],
             cleanup = False,
             clauses = [Catch (C.Null (PointerType (IntegerType 8) (AddrSpace 0)))],
             metadata = []
           },
           [lli|landingpad { i8*, i32 } catch i8* null|]),
          ("landingpad-filter",
           LandingPad {
             type' = StructureType False [
                PointerType (IntegerType 8) (AddrSpace 0),
                IntegerType 32
               ],
             cleanup = False,
             clauses = [Filter (C.Null (ArrayType 1 (PointerType (IntegerType 8) (AddrSpace 0))))],
             metadata = []
           },
           [lli|landingpad { i8*, i32 } filter [1 x i8*] zeroinitializer|]),
          ("landingpad-catch-cleanup",
           LandingPad {
             type' = StructureType False [
                PointerType (IntegerType 8) (AddrSpace 0),
                IntegerType 32
               ],
             cleanup = True,
             clauses = [Catch (C.Null (PointerType (IntegerType 8) (AddrSpace 0)))],
             metadata = []
           },
           [lli|landingpad { i8*, i32 } cleanup catch i8* null|]),
          ("landingpad-filter-cleanup",
           LandingPad {
             type' = StructureType False [
                PointerType (IntegerType 8) (AddrSpace 0),
                IntegerType 32
               ],
             cleanup = True,
             clauses = [Filter (C.Null (ArrayType 1 (PointerType (IntegerType 8) (AddrSpace 0))))],
             metadata = []
           },
           [lli|landingpad { i8*, i32 } cleanup filter [1 x i8*] zeroinitializer|]),
          ("icmp-eq",
           ICmp { iPredicate = IPred.EQ, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp eq i32 %0, %0|]),
          ("icmp-ne",
           ICmp { iPredicate = IPred.NE, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp ne i32 %0, %0|]),
          ("icmp-ugt",
           ICmp { iPredicate = IPred.UGT, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp ugt i32 %0, %0|]),
          ("icmp-uge",
           ICmp { iPredicate = IPred.UGE, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp uge i32 %0, %0|]),
          ("icmp-ult",
           ICmp { iPredicate = IPred.ULT, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp ult i32 %0, %0|]),
          ("icmp-ule",
           ICmp { iPredicate = IPred.ULE, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp ule i32 %0, %0|]),
          ("icmp-sgt",
           ICmp { iPredicate = IPred.SGT, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp sgt i32 %0, %0|]),
          ("icmp-sge",
           ICmp { iPredicate = IPred.SGE, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp sge i32 %0, %0|]),
          ("icmp-slt",
           ICmp { iPredicate = IPred.SLT, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp slt i32 %0, %0|]),
          ("icmp-sle",
           ICmp { iPredicate = IPred.SLE, operand0 = a i32 0, operand1 = a i32 0, metadata = [] },
           [lli|icmp sle i32 %0, %0|]),
          ("fcmp-false",
           FCmp { fpPredicate = FPPred.False, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp false float %1, %1|]),
          ("fcmp-oeq",
           FCmp { fpPredicate = FPPred.OEQ, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp oeq float %1, %1|]),
          ("fcmp-ogt",
           FCmp { fpPredicate = FPPred.OGT, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp ogt float %1, %1|]),
          ("fcmp-oge",
           FCmp { fpPredicate = FPPred.OGE, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp oge float %1, %1|]),
          ("fcmp-olt",
           FCmp { fpPredicate = FPPred.OLT, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp olt float %1, %1|]),
          ("fcmp-ole",
           FCmp { fpPredicate = FPPred.OLE, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp ole float %1, %1|]),
          ("fcmp-one",
           FCmp { fpPredicate = FPPred.ONE, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp one float %1, %1|]),
          ("fcmp-ord",
           FCmp { fpPredicate = FPPred.ORD, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp ord float %1, %1|]),
          ("fcmp-uno",
           FCmp { fpPredicate = FPPred.UNO, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp uno float %1, %1|]),
          ("fcmp-ueq",
           FCmp { fpPredicate = FPPred.UEQ, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp ueq float %1, %1|]),
          ("fcmp-ugt",
           FCmp { fpPredicate = FPPred.UGT, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp ugt float %1, %1|]),
          ("fcmp-uge",
           FCmp { fpPredicate = FPPred.UGE, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp uge float %1, %1|]),
          ("fcmp-ult",
           FCmp { fpPredicate = FPPred.ULT, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp ult float %1, %1|]),
          ("fcmp-ule",
           FCmp { fpPredicate = FPPred.ULE, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp ule float %1, %1|]),
          ("fcmp-une",
           FCmp { fpPredicate = FPPred.UNE, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp une float %1, %1|]),
          ("fcmp-true",
           FCmp { fpPredicate = FPPred.True, operand0 = a float 1, operand1 = a float 1, metadata = [] },
           [lli|fcmp true float %1, %1|]),
          ("store",
          Store {
            volatile = False,
            address = a (ptr i32) 2,
            value = a i32 0,
            maybeAtomicity = Nothing,
            alignment = 0,
            metadata = []
          },
          [lli|store i32 %0, i32* %2|]),
         ("fence",
          Fence {
            atomicity = (System, Acquire),
            metadata = []
          },
          [lli|fence acquire|]),
          ("call",
           Call {
             tailCallKind = Nothing,
             callingConvention = CC.C,
             returnAttributes = [],
             function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32, float, ptr i32, i64, i1, (VectorType 2 i32), (StructureType False [i32, i32])] False)) (UnName 0))),
             arguments = [ (LocalReference i32 (UnName 0), [])
                         , (LocalReference float (UnName 1), [])
                         , (LocalReference (ptr i32) (UnName 2), [])
                         , (LocalReference i64 (UnName 3), [])
                         , (LocalReference i1 (UnName 4), [])
                         , (LocalReference (VectorType 2 i32) (UnName 5), [])
                         , (LocalReference (StructureType False [i32, i32]) (UnName 6), [])
                         ],
             functionAttributes = [],
             metadata = []
           },
           [lli|call void @0(i32 %0, float %1, i32* %2, i64 %3, i1 %4, <2 x i32> %5, { i32, i32 } %6)|]),
          ("call with nested constant expression",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ (ConstantOperand C.Add
                                            { C.nsw = False
                                            , C.nuw = False
                                            , C.operand0 =
                                              C.Mul
                                              { C.nsw = False
                                              , C.nuw = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 5
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 10
                                                }
                                              }
                                            , C.operand1 =
                                              C.Int
                                              { C.integerBits = 32
                                              , C.integerValue = 2
                                              }
                                            }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 add (i32 mul (i32 5, i32 10), i32 2))|]),
          ("call with constant add",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ (ConstantOperand C.Add
                                            { C.nsw = False
                                            , C.nuw = False
                                            , C.operand0 =
                                              C.Int
                                              { C.integerBits = 32
                                              , C.integerValue = 1
                                              }
                                            , C.operand1 =
                                              C.Int
                                              { C.integerBits = 32
                                              , C.integerValue = 2
                                              }
                                            }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 add (i32 1, i32 2))|]),
          ("call with constant add nsw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Add
                                              { C.nsw = True
                                              , C.nuw = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 add nsw (i32 1, i32 2))|]),
          ("call with constant add nuw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Add
                                              { C.nsw = False
                                              , C.nuw = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 add nuw (i32 1, i32 2))|]),
          ("call with constant add nsw nuw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Add
                                              { C.nsw = True
                                              , C.nuw = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 add nsw nuw (i32 1, i32 2))|]),
          ("call with constant fadd",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [float] False)) (Name "myfunc3"))),
              arguments = [ ( ConstantOperand C.FAdd
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.5
                                                }
                                              , C.operand1 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.25
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc3(float fadd (float 0.5, float 0.25))|]),
          ("call with constant sub",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Sub
                                              { C.nsw = False
                                              , C.nuw = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 sub (i32 1, i32 2))|]),
          ("call with constant sub nsw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Sub
                                              { C.nsw = True
                                              , C.nuw = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 sub nsw (i32 1, i32 2))|]),
          ("call with constant sub nuw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Sub
                                              { C.nsw = False
                                              , C.nuw = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 sub nuw (i32 1, i32 2))|]),
          ("call with constant sub nsw nuw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Sub
                                              { C.nsw = True
                                              , C.nuw = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 sub nsw nuw (i32 1, i32 2))|]),
          ("call with constant fsub",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [float] False)) (Name "myfunc3"))),
              arguments = [ ( ConstantOperand C.FSub
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.5
                                                }
                                              , C.operand1 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.25
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc3(float fsub (float 0.5, float 0.25))|]),
          ("call with constant mul",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Mul
                                              { C.nsw = False
                                              , C.nuw = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 mul (i32 1, i32 2))|]),
          ("call with constant mul nsw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Mul
                                              { C.nsw = True
                                              , C.nuw = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 mul nsw (i32 1, i32 2))|]),
          ("call with constant mul nuw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Mul
                                              { C.nsw = False
                                              , C.nuw = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 mul nuw (i32 1, i32 2))|]),
          ("call with constant mul nsw nuw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Mul
                                              { C.nsw = True
                                              , C.nuw = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 mul nsw nuw (i32 1, i32 2))|]),
          ("call with constant fmul",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [float] False)) (Name "myfunc3"))),
              arguments = [ ( ConstantOperand C.FMul
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.5
                                                }
                                              , C.operand1 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.25
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc3(float fmul (float 0.5, float 0.25))|]),
          ("call with constant udiv",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.UDiv
                                              { C.exact = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 4
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 udiv (i32 4, i32 2))|]),
          ("call with constant udiv exact",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.UDiv
                                              { C.exact = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 4
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 udiv exact (i32 4, i32 2))|]),
          ("call with constant sdiv",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.SDiv
                                              { C.exact = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 4
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 sdiv (i32 4, i32 2))|]),
          ("call with constant sdiv exact",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.SDiv
                                              { C.exact = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 4
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 sdiv exact (i32 4, i32 2))|]),
          ("call with constant fdiv",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [float] False)) (Name "myfunc3"))),
              arguments = [ ( ConstantOperand C.FDiv
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 1.5
                                                }
                                              , C.operand1 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.5
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc3(float fdiv (float 1.5, float 0.5))|]),
          ("call with constant urem",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.URem
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 4
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 3
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 urem (i32 4, i32 3))|]),
          ("call with constant urem",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.SRem
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 4
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 3
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 srem (i32 4, i32 3))|]),
          ("call with constant frem",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [float] False)) (Name "myfunc3"))),
              arguments = [ ( ConstantOperand C.FRem
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 1.5
                                                }
                                              , C.operand1 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.5
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc3(float frem (float 1.5, float 0.5))|]),
          ("call with constant shl",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Shl
                                              { C.nsw = False
                                              , C.nuw = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 shl (i32 1, i32 2))|]),
          ("call with constant shl nsw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Shl
                                              { C.nsw = True
                                              , C.nuw = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 shl nsw (i32 1, i32 2))|]),
          ("call with constant shl nuw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Shl
                                              { C.nsw = False
                                              , C.nuw = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                              , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 shl nuw (i32 1, i32 2))|]),
          ("call with constant shl nsw nuw",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Shl
                                              { C.nsw = True
                                              , C.nuw = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 shl nsw nuw (i32 1, i32 2))|]),
          ("call with constant lshr",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.LShr
                                              { C.exact = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 lshr (i32 1, i32 2))|]),
          ("call with constant lshr exact",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.LShr
                                              { C.exact = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 lshr exact (i32 1, i32 2))|]),
          ("call with constant ashr",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.AShr
                                              { C.exact = False
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 ashr (i32 1, i32 2))|]),
          ("call with constant ashr exact",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.AShr
                                              { C.exact = True
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 ashr exact (i32 1, i32 2))|]),
          ("call with constant and",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.And
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 and (i32 1, i32 2))|]),
          ("call with constant or",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Or
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 or (i32 1, i32 2))|]),
          ("call with constant xor",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Xor
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 xor (i32 1, i32 2))|]),
          ("call with constant getelementptr",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [ptr i8] False)) (Name "myfunc"))),
              arguments = [ ( ConstantOperand C.GetElementPtr
                                              { C.inBounds = False
                                              , C.address =
                                                C.GlobalReference
                                                  (ptr
                                                      ArrayType
                                                      { nArrayElements = 4
                                                      , elementType = i8
                                                      })
                                                  (Name "myglobal_str")
                                              , C.indices =
                                                [ C.Int
                                                  { C.integerBits = 32
                                                  , C.integerValue = 0
                                                  }
                                                , C.Int
                                                  { C.integerBits = 32
                                                  , C.integerValue = 0
                                                  }
                                                ]
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc(i8* getelementptr ([4 x i8]* @myglobal_str, i32 0, i32 0))|]),
          ("call with constant getelementptr inbounds",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [ptr i8] False)) (Name "myfunc"))),
              arguments = [ ( ConstantOperand C.GetElementPtr
                                              { C.inBounds = True
                                              , C.address =
                                                C.GlobalReference
                                                  (ptr
                                                    ArrayType
                                                    { nArrayElements = 4
                                                    , elementType = i8
                                                    })
                                                  (Name "myglobal_str")
                                              , C.indices =
                                                [ C.Int
                                                  { C.integerBits = 32
                                                  , C.integerValue = 0
                                                  }
                                                , C.Int
                                                  { C.integerBits = 32
                                                  , C.integerValue = 0
                                                  }
                                                ]
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc(i8* getelementptr inbounds ([4 x i8]* @myglobal_str, i32 0, i32 0))|]),
          ("call with constant trunc",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i8] False)) (Name "myfunc4"))),
              arguments = [ ( ConstantOperand C.Trunc
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 257
                                                }
                                              , C.type' = i8
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc4(i8 trunc (i32 257 to i8))|]),
          ("call with constant zext",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.ZExt
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 8
                                                , C.integerValue = 2
                                                }
                                              , C.type' = i32
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 zext (i8 2 to i32))|]),
          ("call with constant sext",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.SExt
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 8
                                                , C.integerValue = 2
                                                }
                                              , C.type' = i32
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 sext (i8 2 to i32))|]),
          ("call with constant fptoui",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.FPToUI
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 123.0
                                                }
                                              , C.type' = i32
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 fptoui (float 123.0 to i32))|]),
          ("call with constant fptosi",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.FPToSI
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 123.0
                                                }
                                              , C.type' = i32
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 fptosi (float 123.0 to i32))|]),
          ("call with constant uitofp",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [float] False)) (Name "myfunc3"))),
              arguments = [ ( ConstantOperand C.UIToFP
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 123
                                                }
                                              , C.type' = float
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc3(float uitofp (i32 123 to float))|]),
          ("call with constant sitofp",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [float] False)) (Name "myfunc3"))),
              arguments = [ ( ConstantOperand C.SIToFP
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 123
                                                }
                                              , C.type' = float
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc3(float sitofp (i32 123 to float))|]),
          ("call with constant fptrunc",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [float] False)) (Name "myfunc3"))),
              arguments = [ ( ConstantOperand C.FPTrunc
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Double 123.0
                                                }
                                              , C.type' = float
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc3(float fptrunc (double 123.0 to float))|]),
          ("call with constant fpext",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [double] False)) (Name "myfunc5"))),
              arguments = [ ( ConstantOperand C.FPExt
                                              { C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 123.0
                                                }
                                              , C.type' = double
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc5(double fpext (float 123.0 to double))|]),
          ("call with constant ptrtoint",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.PtrToInt
                                              { C.operand0 = C.GlobalReference (ptr i8) (Name "myptr")
                                              , C.type' = i32
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 ptrtoint (i8* @myptr to i32))|]),
          ("call with constant inttoptr",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [ptr i8] False)) (Name "myfunc"))),
              arguments = [ ( ConstantOperand C.IntToPtr
                                              { C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 4
                                                }
                                              , C.type' = ptr i8
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc(i8* inttoptr (i32 4 to i8*))|]),
          ("call with constant bitcast",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [ptr i8] False)) (Name "myfunc"))),
              arguments = [ ( ConstantOperand
                                C.BitCast
                                { C.operand0 = (C.GlobalReference (ptr i1) (Name "myglobal"))
                                , C.type' = (ptr i8)
                                }
                            , [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc(i8* bitcast (i1* @myglobal to i8*))|]),
          ("call with constant addrspacecast",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [PointerType (IntegerType 8) (AddrSpace 1)] False)) (Name "myfunc6"))),
              arguments = [ ( ConstantOperand
                                C.AddrSpaceCast
                                { C.operand0 = (C.GlobalReference (ptr i32) (Name "myglobal"))
                                , C.type' = PointerType (IntegerType 8) (AddrSpace 1)
                                }
                            , [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc6(i8 addrspace(1)* addrspacecast (i32* @myglobal to i8 addrspace(1)*))|]),
          ("call with constant icmp eq",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i1] False)) (Name "myfunc7"))),
              arguments = [ (ConstantOperand C.ICmp
                                            { C.iPredicate = IPred.EQ
                                            , C.operand0 =
                                              C.Int
                                              { C.integerBits = 32
                                              , C.integerValue = 4
                                              }
                                            , C.operand1 =
                                              C.Int
                                              { C.integerBits = 32
                                              , C.integerValue = 1
                                              }
                                            }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc7(i1 icmp eq (i32 4, i32 1))|]),
          ("call with constant icmp ne",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i1] False)) (Name "myfunc7"))),
              arguments = [ ( ConstantOperand C.ICmp
                                              { C.iPredicate = IPred.NE
                                              , C.operand0 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 4
                                                }
                                              , C.operand1 =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc7(i1 icmp ne (i32 4, i32 1))|]),
          ("call with constant fcmp oeq",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i1] False)) (Name "myfunc7"))),
              arguments = [ ( ConstantOperand C.FCmp
                                              { C.fpPredicate = FPPred.OEQ
                                              , C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 1.5
                                                }
                                              , C.operand1 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.5
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc7(i1 fcmp oeq (float 1.5, float 0.5))|]),
          ("call with constant fcmp one",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i1] False)) (Name "myfunc7"))),
              arguments = [ ( ConstantOperand C.FCmp
                                              { C.fpPredicate = FPPred.ONE
                                              , C.operand0 =
                                                C.Float
                                                { C.floatValue = Float.Single 1.5
                                                }
                                              , C.operand1 =
                                                C.Float
                                                { C.floatValue = Float.Single 0.5
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc7(i1 fcmp one (float 1.5, float 0.5))|]),
          ("call with constant select",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.Select
                                              { C.condition' =
                                                C.Int
                                                { C.integerBits = 1
                                                , C.integerValue = 0
                                                }
                                              , C.trueValue =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              , C.falseValue =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 2
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 select (i1 false, i32 1, i32 2))|]),
          ("call with constant extractelement",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.ExtractElement
                                              { C.vector =
                                                C.Undef
                                                { C.constantType =
                                                  VectorType
                                                  { nVectorElements = 4
                                                  , elementType = i32
                                                  }
                                                }
                                              , C.index =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 1
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 extractelement (<4 x i32> undef, i32 1))|]),
          ("call with constant insertelement",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [VectorType { nVectorElements = 5, elementType = i32 }] False)) (Name "myfunc8"))),
              arguments = [ ( ConstantOperand C.InsertElement
                                              { C.vector =
                                                C.Undef
                                                { C.constantType =
                                                  VectorType
                                                  { nVectorElements = 5
                                                  , elementType = i32
                                                  }
                                                }
                                              , C.element =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 35
                                                }
                                              , C.index =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 0
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc8(<5 x i32> insertelement (<5 x i32> undef, i32 35, i32 0))|]),
          ("call with constant shufflevector",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [VectorType { nVectorElements = 3, elementType = i32 }] False)) (Name "myfunc9"))),
              arguments = [ ( ConstantOperand C.ShuffleVector
                                              { C.operand0 =
                                                C.Undef
                                                { C.constantType =
                                                  VectorType
                                                  { nVectorElements = 3
                                                  , elementType = i32
                                                  }
                                                }
                                              , C.operand1 =
                                                C.Undef
                                                { C.constantType =
                                                  VectorType
                                                  { nVectorElements = 3
                                                  , elementType = i32
                                                  }
                                                }
                                              , C.mask =
                                                C.Vector
                                                { C.memberValues =
                                                  [ C.Int
                                                    { C.integerBits = 32
                                                    , C.integerValue = 0
                                                    }
                                                  , C.Int
                                                    { C.integerBits = 32
                                                    , C.integerValue = 4
                                                    }
                                                  , C.Int
                                                    { C.integerBits = 32
                                                    , C.integerValue = 1
                                                    }
                                                  ]
                                                }
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc9(<3 x i32> shufflevector (<3 x i32> undef, <3 x i32> undef, <3 x i32> <i32 0, i32 4, i32 1>))|]),
          ("call with constant extractvalue",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32] False)) (Name "myfunc2"))),
              arguments = [ ( ConstantOperand C.ExtractValue
                                              { C.aggregate =
                                                C.Struct
                                                { C.structName = Nothing
                                                , C.isPacked = False
                                                , C.memberValues =
                                                  [ C.Int
                                                    { C.integerBits = 32
                                                    , C.integerValue = 23
                                                    }
                                                  , C.Int
                                                    { C.integerBits = 8
                                                    , C.integerValue = 2
                                                    }
                                                  ]
                                                }
                                              , C.indices' = [0]
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc2(i32 extractvalue ({i32, i8} {i32 23, i8 2}, 0))|]),
          ("call with constant insertvalue",
            Call {
              tailCallKind = Nothing,
              callingConvention = CC.C,
              returnAttributes = [],
              function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [StructureType {isPacked = False, elementTypes = [i32, i8]}] False)) (Name "myfunc10"))),
              arguments = [ ( ConstantOperand C.InsertValue
                                              { C.aggregate =
                                                C.Struct
                                                { C.structName = Nothing
                                                , C.isPacked = False
                                                , C.memberValues =
                                                  [ C.Int
                                                    { C.integerBits = 32
                                                    , C.integerValue = 41
                                                    }
                                                  , C.Int
                                                    { C.integerBits = 8
                                                    , C.integerValue = 2
                                                    }
                                                  ]
                                                }
                                              , C.element =
                                                C.Int
                                                { C.integerBits = 32
                                                , C.integerValue = 23
                                                }
                                              , C.indices' = [0]
                                              }, [])
                          ],
              functionAttributes = [],
              metadata = []
            },
            [lli|call void @myfunc10({i32, i8} insertvalue ({i32, i8} {i32 41, i8 2}, i32 23, 0))|])
         ]
   ],

   testGroup "terminators" [
    testCase name $ mASTQ @?= mAST
    | (name, mAST, mASTQ) <- [
     (
       "ret",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "entry") [
             ] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       [llmod|; ModuleID = '<string>'

       define void @0() {
       entry:
         ret void
       }|]
     ), (
       "br",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "entry") [] (
              Do $ Br (Name "foo") []
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       [llmod|; ModuleID = '<string>'

       define void @0() {
       entry:
         br label %foo

       foo:                                              ; preds = %0
         ret void
       }|]
     ), (
       "condbr",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "bar") [] (
              Do $ CondBr (ConstantOperand (C.Int 1 1)) (Name "foo") (Name "bar") []
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             )
           ]
          }
        ],
       [llmod|; ModuleID = '<string>'

       define void @0() {
       bar:
         br i1 true, label %foo, label %bar

       foo:                                              ; preds = %bar
         ret void
       }|]
     ), (
       "switch",
       Module "<string>" "<string>" Nothing Nothing [
         GlobalDefinition $ functionDefaults {
           G.returnType = VoidType,
           G.name = UnName 0,
           G.basicBlocks = [
             BasicBlock (Name "entry") [] (
               Do $ Switch {
                 operand0' = ConstantOperand (C.Int 16 2),
                 defaultDest = Name "foo",
                 dests = [
                  (C.Int 16 0, Name "entry"),
                  (C.Int 16 2, Name "foo"),
                  (C.Int 16 3, Name "entry")
                 ],
                 metadata' = []
              }
             ),
             BasicBlock (Name "foo") [] (
               Do $ Ret Nothing []
              )
            ]
          }
        ],
       [llmod|; ModuleID = '<string>'

       define void @0() {
       entry:
         switch i16 2, label %foo [
           i16 0, label %entry
           i16 2, label %foo
           i16 3, label %entry
         ]

       foo:
         ret void
       }|]
     ), (
       "indirectbr",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ globalVariableDefaults {
          G.name = UnName 0,
          G.type' = PointerType (IntegerType 8) (AddrSpace 0),
          G.initializer = Just (C.BlockAddress (Name "foo") (Name "end"))
        },
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = Name "foo",
          G.basicBlocks = [
            BasicBlock (Name "entry") [
              UnName 0 := Load {
                       volatile = False,
                       address = ConstantOperand (C.GlobalReference (ptr (ptr i8)) (UnName 0)),
                       maybeAtomicity = Nothing,
                       alignment = 0,
                       metadata = []
                     }
            ] (
              Do $ IndirectBr {
                operand0' = LocalReference (ptr i8) (UnName 0),
                possibleDests = [Name "end"],
                metadata' = []
             }
            ),
            BasicBlock (Name "end") [] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
--       \  indirectbr i8* null, [label %foo]\n\
       [llmod|; ModuleID = '<string>'

       @0 = global i8* blockaddress(@foo, %end)

       define void @foo() {
       entry:
         %0 = load i8** @0
         indirectbr i8* %0, [label %end]

       end:
         ret void
       }|]
     ), (
       "invoke",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.parameters = ([
            Parameter (IntegerType 32) (Name "a") [],
            Parameter (IntegerType 16) (Name "b") []
           ], False),
          G.basicBlocks = [
            BasicBlock (Name "entry") [] (
              Do $ Invoke {
               callingConvention' = CC.C,
               returnAttributes' = [],
               function' = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType void [i32, i16] False)) (UnName 0))),
               arguments' = [
                (ConstantOperand (C.Int 32 4), []),
                (ConstantOperand (C.Int 16 8), [])
               ],
               functionAttributes' = [],
               returnDest = Name "foo",
               exceptionDest = Name "bar",
               metadata' = []
              }
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             ),
            BasicBlock (Name "bar") [
             UnName 0 := LandingPad {
               type' = StructureType False [
                  PointerType (IntegerType 8) (AddrSpace 0),
                  IntegerType 32
                 ],
               cleanup = True,
               clauses = [Catch (C.Null (PointerType (IntegerType 8) (AddrSpace 0)))],
               metadata = []
             }
             ] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       [llmod|; ModuleID = '<string>'

       define void @0(i32 %a, i16 %b) {
       entry:
         invoke void @0(i32 4, i16 8)
                 to label %foo unwind label %bar

       foo:
         ret void

       bar:
         %0 = landingpad { i8*, i32 }
                 cleanup
                 catch i8* null
         ret void
       }|]
     ), (
       "resume",
       Module "<string>" "<string>" Nothing Nothing [
         GlobalDefinition $ functionDefaults {
           G.returnType = VoidType,
           G.name = UnName 0,
           G.basicBlocks = [
             BasicBlock (Name "entry") [] (
               Do $ Resume (ConstantOperand (C.Int 32 1)) []
              )
            ]
          }
        ],
       [llmod|; ModuleID = '<string>'

       define void @0() {
       entry:
         resume i32 1
       }|]
     ), (
       "unreachable",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = VoidType,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "entry") [] (
              Do $ Unreachable []
             )
           ]
         }
        ],
       [llmod|; ModuleID = '<string>'

       define void @0() {
       entry:
         unreachable
       }|]
     )
    ]
   ]
 ]
