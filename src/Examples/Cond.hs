module Examples.Cond ( simpleIfProg
                     , ifVarProg
                     , simpleInfProg
                     ) where

import Nightfall.Lang.Types

-- * Simple program that uses one if / else statement

-- | Haskell program
{-
simpleIf :: Felt
simpleIf = if 4 == 8
           then 10
           else 20
-}

-- | EDSL version
simpleIfStmts :: [Statement]
simpleIfStmts = [ comment "Simple, stupid and trivial program that makes uses of a condition"
                , comment "if (4 == 8) then return 10 else return 20"
                , comment "Should return 20"
                , ifElse (eq 4 8) [ret . Just $ 10] [ret . Just $ 20]
                ]

simpleIfProg :: ZKProgram
simpleIfProg = mkSimpleProgram "simple if" simpleIfStmts

-- * Simple program that uses one if / else statement on a moderately complex computation involving variables

-- | Haskell program
{-
simpleIf :: Felt
simpleIf = let a = 145
               b = 79
               target = 203
               sum = a + b
               okVal = 10
               nokVal = 20
            in if sum == target
               then okVal
               else nokVal
-}

-- | EDSL version
ifVarStmts :: [Statement]
ifVarStmts = [ comment "Makes a if/else comparison on a moderately complex computation, involving variables"
             , comment "It sums a=145 + b=79 and compares equality with target=203."
             , comment "If equal, it returns okVal=10, otherwise nokVal=20"
             , comment "It should return 20"
             , declareVarF "a" 145
             , declareVarF "b" 79
             , declareVarF "target" 203
             , declareVarF "sum" (varF "a" + varF "b")
             , declareVarF "okVal" 10
             , declareVarF "nokVal" 20
             , ifElse (varF "sum" `eq` varF "target") [ret . Just $ varF "okVal"] [ret . Just $ varF "nokVal"]
            ]

ifVarProg :: ZKProgram
ifVarProg = mkSimpleProgram "If with vars" ifVarStmts

-- * Simple program that compares two fixed numbers stored in variables and return the lowest

{-
simpleInf :: Felt
simpleInf = let n1 = 4238
                n2 = 21987
            in if n1 <= n2
               then n1
               else n2
-}

-- | EDSL version
simpleInfStmts :: [Statement]
simpleInfStmts = [ comment "if n1=4238 <= n2=21987 then n1 else n2."
                 , comment "It should return 4238"
                 , emptyLine
                 , declareVarF "n1" 4238
                 , declareVarF "n2" 21987
                 , ifElse (varF "n1" `lte` varF "n2") [ret . Just $ varF "n1"] [ret . Just $ varF "n2"]
                 ]

simpleInfProg :: ZKProgram
simpleInfProg = mkSimpleProgram "simple inf" simpleInfStmts