module Examples.Fun ( collatzFixedProg
                    , collatzPrivProg
                    ) where

import Nightfall.Lang.Types


-- * The Collatz Sequence, start from 10

-- Haskell version
{-
collatzFixed :: Felt
collatzFixed = collatz' 10
    where collataz' :: Felt -> Felt
          collatz 1 = 1
          collatz n | odd n = collatz (3 * n + 1)
                    | otherwise = collatz (n `div` 2)
-}

-- | DSL version
collatzFixedStmts :: [Statement]
collatzFixedStmts = [ comment "Compute the Collatz sequence, starting from a fixed position: 10 and returns the length of the sequence."
                    , comment "It should return 7"
                    , emptyLine
                    , declareVarF "start" 10
                    , declareVarF "length" 1
                    , declareVarF "n" (varF "start")
                    , while (varF "n" `gt` 1) [ assignVarF "length" (varF "length" + 1)
                                              , ifElse (isOdd (varF "n"))
                                                 [assignVarF "n" (varF "n" * 3 + 1)]
                                                 [assignVarF "n" (varF "n" `div'` 2)]
                                                ]
                    , ret . Just $ varF "length"
                     ]

collatzFixedProg :: ZKProgram
collatzFixedProg = mkSimpleProgram "Fixed Collatz (10)" collatzFixedStmts

-- * The Collatz Sequence, but the starting number comes from the private inputs

-- Haskell version
{-
collatzPriv :: Felt -> Felt
collatzPriv n = collatz' n
    where collataz' :: Felt -> Felt
          collatz 1 = 1
          collatz n | odd n = collatz (3 * n + 1)
                    | otherwise = collatz (n `div` 2)
-}

-- | DSL version
collatzPrivStmts :: [Statement]
collatzPrivStmts = [ comment "Compute the Collatz sequence, starting position taken from secret input"
                  , comment "It returns the length of the sequence"
                  , emptyLine
                  , declareVarF "start" nextSecretF
                  , declareVarF "n" (varF "start")
                  , declareVarF "length" 1
                  , while (varF "n" `gt` 1) [ incVarF "length" 1
                                            , ifElse (isOdd (varF "n"))
                                             --   [assignVarF "n" (varF "n" * 3 + 1)]
                                                [updateVarF "n" $ \n -> n * 3 + 1]
                                             --   [assignVarF "n" (varF "n" `div'` 2)]
                                                [updateVarF "n" $ \n -> n `div'` 2]
                                            ]
                  , ret . Just $ varF "length"
                  ]

collatzPrivProg :: ZKProgram
collatzPrivProg = mkZKProgram "collatz private" collatzPrivStmts [] "collatz_priv_secrets.input"