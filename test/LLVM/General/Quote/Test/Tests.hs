module LLVM.General.Quote.Test.Tests where

import Test.Tasty

import qualified LLVM.General.Quote.Test.Constants as Constants
import qualified LLVM.General.Quote.Test.DataLayout as DataLayout
import qualified LLVM.General.Quote.Test.InlineAssembly as InlineAssembly
import qualified LLVM.General.Quote.Test.Instructions as Instructions
import qualified LLVM.General.Quote.Test.Loops as Loops
import qualified LLVM.General.Quote.Test.Metadata as Metadata

tests = testGroup "language-llvm-quote"
  [ Constants.tests
  , DataLayout.tests
  , InlineAssembly.tests
  , Instructions.tests
  , Loops.tests
  , Metadata.tests
  ]