{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nightfall.MASM.Types where

import Control.Monad.Writer.Strict
import qualified Data.DList as DList
import Data.Map (Map)
import Data.String
import Data.Text.Lazy (Text)
import Data.Typeable
import Data.Word (Word32)
import qualified GHC.Exts
import GHC.Generics

type ProcName = Text

type ModName = Text

data Module = Module
  { moduleImports :: [ModName],
    moduleProcs :: Map ProcName Proc,
    moduleProg :: Program
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

data Proc = Proc
  { procNLocals :: Int,
    procInstrs :: [Instruction]
  }
  deriving (Eq, Ord, Show, Generic, Typeable)

newtype Program = Program {programInstrs :: [Instruction]}
  deriving (Eq, Ord, Show, Generic, Typeable)

data Instruction
  = Exec ProcName -- exec.foo
  | If
      { -- if.true
        thenBranch :: [Instruction],
        elseBranch :: [Instruction]
      }
  | While [Instruction] -- while.true
  | AdvPush Word32 -- adv_push.n
  | Push Word32 -- push.n
  | Swap Word32 -- swap[.i]
  | Drop -- drop
  | CDrop -- cdrop
  | Dup Word32 -- dup.n
  | MoveUp Word32 -- movup.n
  | MoveDown Word32 -- movdn.n
  | TruncateStack -- exec.sys::truncate_stack
  | SDepth -- sdepth
  | Eq (Maybe Word32) -- eq[.n]
  | NEq (Maybe Word32) -- neq[.n]
  | Lt -- lt
  | Lte -- lte
  | Gt -- gt
  | Gte -- gte
  | Not -- not
  | IsOdd -- is_odd
  | LocStore Word32 -- loc_store.i
  | LocLoad Word32 -- loc_load.i
  | MemLoad (Maybe Word32) -- mem_load[.i]
  | MemStore (Maybe Word32) -- mem_store[.i]
  | Add (Maybe Word32) -- add[.n]
  | Sub (Maybe Word32) -- sub[.n]
  | Mul (Maybe Word32) -- mul[.n]
  | Div (Maybe Word32) -- div[.n]
  | Neg
  | IAdd -- u32checked_add
  | ISub -- "u32checked_sub"
  | IMul -- u32checked_mul
  | IDiv -- u32checked_div
  | IMod -- u32checked_mod
  | IDivMod (Maybe Word32) -- u32checked_divmod
  | IShL
  | IShR -- u32checked_{shl, shr}
  | IAnd
  | IOr
  | IXor
  | INot -- u32checked_{and, or, xor, not}
  | IEq (Maybe Word32)
  | INeq -- u32checked_{eq[.n], neq}
  | ILt
  | IGt
  | ILte
  | IGte -- u32checked_{lt[e], gt[e]}
  | IRotl
  | IRotr
  | IPopcnt -- u32checked_popcnt
  | -- "faked 64 bits" operations, u64::checked_{add,sub,mul}
    IAdd64
  | ISub64
  | IMul64
  | IDiv64
  | IMod64
  | IShL64
  | IShR64
  | IOr64
  | IAnd64
  | IXor64
  | IEq64
  | IEqz64
  | INeq64
  | ILt64
  | IGt64
  | ILte64
  | IGte64
  | IRotl64
  | IRotr64
  | Assert
  | AssertZ
  | Comment Text
  | EmptyL     -- ability to insert empty line for spacing (purely decorative and for easier inspection)
  deriving (Eq, Ord, Show, Generic, Typeable)

newtype PpMASM a = PpMASM {runPpMASM :: Writer (DList.DList String) a}
  deriving (Generic, Typeable, Functor, Applicative, Monad)

deriving instance MonadWriter (DList.DList String) PpMASM

instance (a ~ ()) => IsString (PpMASM a) where
  fromString s = tell [s]

instance (a ~ ()) => GHC.Exts.IsList (PpMASM a) where
  type Item (PpMASM a) = String
  fromList = tell . DList.fromList
  toList = DList.toList . snd . runWriter . runPpMASM
