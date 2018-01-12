{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

import LLVM.Prelude
import LLVM.AST as AST
import LLVM.AST.Name as AST
import LLVM.Quote.LLVM as Q
import Data.String (fromString)

example :: AST.Module
example = [Q.llmod|
; ModuleID = 'simple module'

define i32 @foo(i32 %x) {
entry:
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr
  ret i32 1001
}
|]
