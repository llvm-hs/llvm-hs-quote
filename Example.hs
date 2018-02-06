{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Example (
  example1,
  example2,
  example3,
  example4,
) where

import LLVM.AST as AST
import LLVM.Quote.LLVM as Q

-- | Module quotation
example1 :: AST.Module
example1 = [Q.llmod|
; ModuleID = 'simple module'

define i32 @foo(i32 %x) {
entry:
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr
  ret i32 1001
}
|]

-- | Instruction antiquotation
example2 :: Either AST.Instruction AST.Terminator -> AST.Module
example2 instructionOrTerminator = [Q.llmod|
  ; ModuleID = 'simple module'
  define i32 @myfunc(){
  entry:
    $instr:instructionOrTerminator
    ret i32 0
  }
|]

-- | Instruction quotation
example3 :: AST.Type -> AST.Instruction
example3 t = [Q.lli|alloca $type:t|]

-- | Definition quotation
example4 :: AST.Definition
example4 = [Q.lldef|@0 = global i32 1|]
