module LLVM.Quote.Test.Tests where

import Test.Tasty

import qualified LLVM.Quote.Test.Constants as Constants
import qualified LLVM.Quote.Test.Constants as Constants
import qualified LLVM.Quote.Test.DataLayout as DataLayout
import qualified LLVM.Quote.Test.DataLayout as DataLayout
import qualified LLVM.Quote.Test.InlineAssembly as InlineAssembly
import qualified LLVM.Quote.Test.InlineAssembly as InlineAssembly
import qualified LLVM.Quote.Test.Instructions as Instructions
import qualified LLVM.Quote.Test.Instructions as Instructions
import qualified LLVM.Quote.Test.Loops as Loops
import qualified LLVM.Quote.Test.Loops as Loops
import qualified LLVM.Quote.Test.Metadata as Metadata
import qualified LLVM.Quote.Test.Metadata as Metadata

tests = testGroup "language-llvm-quote"
  [ Constants.tests
  , DataLayout.tests
  , InlineAssembly.tests
  , Instructions.tests
  , Loops.tests
  , Metadata.tests
  ]
