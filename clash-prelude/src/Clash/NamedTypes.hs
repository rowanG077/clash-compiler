{- |
Copyright  :  (C) 2017, Myrtle Software Ltd, QBayLogic, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Add inline documentation to types:

@
fifo
  :: Clock dom
  -> Reset dom
  -> SNat addrSize
  -> "read request" ::: Signal dom Bool
  -> "write request" ::: Signal dom (Maybe (BitVector dataSize))
  -> ( "q"     ::: Signal dom (BitVector dataSize)
     , "full"  ::: Signal dom Bool
     , "empty" ::: Signal dom Bool
     )
@

which can subsequently be inspected in the interactive environment:

>>> import Clash.Explicit.Prelude
>>> :t fifo @System
fifo @System
  :: Clock System
     -> Reset System
     -> SNat addrSize
     -> ("read request" ::: Signal System Bool)
     -> ("write request" ::: Signal System (Maybe (BitVector dataSize)))
     -> ("q" ::: Signal System (BitVector dataSize),
         "full" ::: Signal System Bool, "empty" ::: Signal System Bool)

-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeOperators  #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.NamedTypes
  ( (:::)
  , name
  )
where

import GHC.TypeLits (Symbol)


type (name :: k) ::: a = a
-- ^ Annotate a type with a name


-- | Name entity (VHDL) or module (Verilog) instantiation (best-effort)
--
-- Do
--
-- @
-- 'name' \@"foo" f x y
-- @
--
-- To label that particular instantiation of /f/ with /foo/
name :: forall (name :: Symbol) a . a -> name ::: a
name = id
{-# NOINLINE name #-}

{- $setup
>>> :set -XDataKinds -XTypeOperators -XNoImplicitPrelude
>>> import Clash.Explicit.Prelude
>>> :{
let fifo
      :: Clock dom
      -> Reset dom
      -> SNat addrSize
      -> "read request" ::: Signal dom Bool
      -> "write request" ::: Signal dom (Maybe (BitVector dataSize))
      -> ( "q"     ::: Signal dom (BitVector dataSize)
         , "full"  ::: Signal dom Bool
         , "empty" ::: Signal dom Bool
         )
    fifo = Clash.Explicit.Prelude.undefined
:}

-}
