module Nightfall.Lang.Internal.Types where

import Data.Word ( Word64 )

-- | Field Element type
type Felt = Word64

type VarName = String

type FunName = String

-- | Expression, internal type, not exposed
data Expr_ =
    -- | Literals
      Lit Felt  -- ^ 309183, 2398713, whatever NOTE: we might want to use explicit types like W32, W64, etc.?
    | Bo Bool   -- ^ true/false, 1/0 

    -- | Arithmetic operations
    | Add Expr_ Expr_ -- ^ a + b
    | Sub Expr_ Expr_ -- ^ a - b
    | Mul Expr_ Expr_ -- ^ a * b
    | Div Expr_ Expr_ -- ^ a / b (integer division)
    | Mod Expr_ Expr_ -- ^ a % b
    | IDiv32 Expr_ Expr_ -- ^ a `quot` b with a and b 32-bit integers

    -- | Boolean operations
    | Equal Expr_ Expr_     -- ^ a == b
    | Not Expr_             -- ^ !a
    | Lower Expr_ Expr_     -- ^ a < b
    | LowerEq Expr_ Expr_   -- ^ a <= b
    | Greater Expr_ Expr_   -- ^ a > b
    | GreaterEq Expr_ Expr_ -- ^ a >= b
    | IsOdd Expr_           -- a `mod` 2 == 1
    
    -- | Variables
    | VarF VarName     -- ^ "calling" a variable of type Felt by its name (e.g. "foo")
    | VarB VarName     -- ^ same, but with boolean variable

    -- | Functions
    | FCall FunName [Expr_]

    -- | Secret Input
    | NextSecret       -- ^ The next available secret input
    deriving (Eq, Show)

-- | Simple, internal type, not exposed
data Statement_ =
    -- | Variable declaration
      DeclVariable VarName Expr_  -- ^ let a = 634
    
    -- | Variable assignment
    | AssignVar VarName Expr_     -- ^ a <- 368

    -- | Conditionals
    | IfElse Expr_ [Statement_] [Statement_] -- ^ condition if-block else-block
    | While Expr_ [Statement_]               -- ^ condition body

    -- | Naked function call
    | NakedCall FunName [Expr_]

    -- | Return
    | Return (Maybe Expr_)

    -- | Comment
    | Comment String

    -- | Allow to add empty lines in the generated code, for clarity
    | EmptyLine
    deriving (Eq, Show)
