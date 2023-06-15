module Examples.Simple ( trivial1Prog
                       , trivial2Prog
                       , trivial3Prog
                       , trivial4Prog
                       , simpleVar1Prog
                       , simpleVar2Prog
                       , simpleVar3Prog
                       ) where

import Nightfall.Lang.Types

-- * Simplest, most trivial program that adds to fixed numbers, no variable, no inputs (public or secret)

-- | That's the Haskell program to describe with the EDSL
{-
trivial1 :: Felt
trivial1 = 33 + 42
-}

-- | That's the EDSL version
trivial1Stmts :: [Statement]
trivial1Stmts = [ comment "Simply add fixed numbers 33 + 42, should output 75"
                , ret . Just $ add (lit 33) (lit 42)
                ]

trivial1Prog :: ZKProgram
trivial1Prog = ZKProgram { pName = "trivial 1"
                          , pStatements = trivial1Stmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }

-- * Almost as simple a program, we just have one nested level

-- | That's the Haskell program to describe with the EDSL
{-
trivial2 :: Felt
trivial2 = 29 + 156 + 14 
-}

-- | That's the EDSL version
trivial2Stmts :: [Statement]
trivial2Stmts = [ comment "Simply add three numbers, 29 + 156 + 14, should output 199"
                , ret . Just $ add (lit 29) (add (lit 156) (lit 14))
                ]

trivial2Prog :: ZKProgram
trivial2Prog = ZKProgram { pName = "trivial 2"
                          , pStatements = trivial2Stmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }


-- * This time we have a multiplication and an addition, we need to make sure orders of evaluation is correct

-- | That's the Haskell program to describe with the EDSL
{-
trivial3 :: Felt
trivial3 = 1238 * (345 + 78)
-}

-- | That's the EDSL version
trivial3Stmts :: [Statement]
trivial3Stmts = [ comment "Perform an addition followed by a multipication, 1238 * (345 + 78), should output 523674"
                , ret . Just $ mul (lit 1238) (add (lit 345) (lit 78))
                ]

trivial3Prog :: ZKProgram
trivial3Prog = ZKProgram { pName = "trivial 3"
                          , pStatements = trivial3Stmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }

-- * Another simple program that uses the Num instance t omake it easier to write and make sure it works

-- | That's the Haskell program to describe with the EDSL
{-
trivial4 :: Felt
trivial4 = 52 * (11 - 1)
-}

-- | That's the EDSL version
trivial4Stmts :: [Statement]
trivial4Stmts = [ comment "Performs 52 * (11 - 1), written with Num instance, should output 520"
                , ret . Just $ 52 * (11 - 1)
                ]

trivial4Prog :: ZKProgram
trivial4Prog = ZKProgram { pName = "trivial 4"
                          , pStatements = trivial4Stmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }

-- * Simple program using a variable to store a value

-- Haskell program
{-
simpleVar1 :: Felt
simpleVar1 = let a = 999
             in a + 1
-}


-- EDSL version
simpleVar1Stmts :: [Statement]
simpleVar1Stmts = [ comment "Simple addition, but with a variable storing a value"
                 , comment "a = 999"
                 , comment "a + 1. It should return 1000"
                 , declareVarF "a" (lit 999)
                 , ret . Just $ add (varF "a") (lit 1)
                 ]

simpleVar1Prog :: ZKProgram
simpleVar1Prog = ZKProgram { pName = "simple var 1"
                          , pStatements = simpleVar1Stmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }


-- * Same program as before, but uses more variables and make a subtraction. Makes sure values are fetched correctly

-- Haskell program
{-
simpleVar2 :: Felt
simpleVar2 = let a = 888
                 b = 222
                 diff = a - b
             in diff
-}


-- EDSL version
simpleVar2Stmts :: [Statement]
simpleVar2Stmts = [ comment "Simple subtraction, but uses three variables"
                 , comment "a = 888, b = 222"
                 , comment "c = a - b. Return c"
                 , comment "It should return 666"
                 , declareVarF "a" 888
                 , declareVarF "b" 222
                 , declareVarF "c" (varF "a" - varF "b")
                 , ret . Just $ varF "c"
                 ]

simpleVar2Prog :: ZKProgram
simpleVar2Prog = ZKProgram { pName = "simple var 2"
                          , pStatements = simpleVar2Stmts
                          , pPublicInputs = []
                          , pSecretInputs = []
                          }

-- * Simple program that overwrite the values of a variable several times

simpleVar3Stmts :: [Statement]
simpleVar3Stmts = [ comment "Rewrite on the same variable several times"
                  , comment "a = 10, b = 20, a = 50, a + b. Should return 70"
                  , declareVarF "a" 10
                  , declareVarF "b" 20
                  , assignVarF "a" 50
                  , ret . Just $ varF "a" + varF "b"
                  ]

simpleVar3Prog :: ZKProgram
simpleVar3Prog = mkSimpleProgram "simple var 3" simpleVar3Stmts