module Main (main) where

import Test.Tasty

import qualified Properties

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "properties" Properties.tests
