module Main (
  main
) where

import Test.Tasty
import qualified LLVM.Quote.Test.Tests as Quote

main :: IO ()
main = defaultMain Quote.tests
