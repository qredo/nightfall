module Properties where

import Test.Tasty

import qualified Properties.Arith as Arith

tests :: [TestTree]
tests =
  [ Arith.tests
  ]
