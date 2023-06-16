{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Properties.Arith where

import Data.Bits ((.|.), shiftL)
import Data.Word
import Test.Tasty.QuickCheck
import Test.Tasty (TestTree)

import Nightfall.Targets.Miden (runZKProgram)
import qualified Nightfall.Lang.Types as Nightfall

goldilocks = 2^64 - 2^32 + 1

data Arith
  = Add Arith Arith
  | Sub Arith Arith
  | Lit Nightfall.Felt
  deriving Show

arithToNightfall :: Arith -> Nightfall.Expr Nightfall.Felt
arithToNightfall = \case
  Lit a -> Nightfall.lit a
  Add a b -> Nightfall.add (arithToNightfall a) (arithToNightfall b)
  Sub a b -> Nightfall.sub (arithToNightfall a) (arithToNightfall b)

evalArith :: Arith -> Integer
evalArith = \case
  Lit a -> fromIntegral a `mod` goldilocks
  Add a b -> (evalArith a + evalArith b) `mod` goldilocks
  Sub a b -> (evalArith a - evalArith b) `mod` goldilocks

randomArith :: Int -> Gen Arith
randomArith 0 = Lit . (fromIntegral :: Word32 -> Nightfall.Felt) <$> arbitrary
randomArith k = frequency
  [ (1, randomArith 0)
  , (7, Add <$> randomArith (k-1) <*> randomArith (k-1))
  , (7, Sub <$> randomArith (k-1) <*> randomArith (k-1))
  ]

logSize :: Integral a => a -> a
logSize = floor @Double . logBase 2 . fromIntegral . (+1)

instance Arbitrary Arith where
  arbitrary = sized (randomArith . logSize)

prop_arith :: Arith -> Property
prop_arith a = ioProperty $ do
  let refResult = evalArith a
      expr = arithToNightfall a
      zkProg = Nightfall.ZKProgram { Nightfall.pName = "testprog"
                                   , Nightfall.pStatements = [Nightfall.ret (Just expr)]
                                   , Nightfall.pPublicInputs = []
                                   , Nightfall.pSecretInputs = ""
                                   }
  -- print expr
  res <- runZKProgram zkProg
  -- print res
  case res of
    Left err -> error err
    Right xs -> case xs of
      (w:_) -> return $ refResult === fromIntegral w
      _ -> error ("unexpected stack: " ++ show xs)

tests :: TestTree
tests = testProperty "translates arithmetic expressions correctly" prop_arith
