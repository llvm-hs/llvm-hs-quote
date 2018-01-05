{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

import LLVM.AST
import LLVM.AST.Name
import LLVM.Prelude
import LLVM.Quote.LLVM
import Data.String (fromString)

example :: Module
example = [llmod|
; ModuleID = 'simple module'

define i32 @foo(i32 %x) {
entry:
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr
  ret i32 1001
}

|]
