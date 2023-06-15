module Examples.Inputs ( simpleSecretProg
                       ) where

import Nightfall.Lang.Types

-- | Simple program that outputs 10x the secret input

-- Haskell version
{-
simpleSecret :: Felt -> Felt
simpleSecret secret = 10 * secret
-}

-- DSL version
simpleSecretStmts :: [Statement]
simpleSecretStmts = [ comment "Simple program that outputs 10x the secret input"
                    , emptyLine
                    , ret . Just $ 10 * nextSecretF
                    ]

simpleSecretProg :: ZKProgram
simpleSecretProg = mkZKProgram "simple secret" simpleSecretStmts [] "simple_secrets.inputs" 