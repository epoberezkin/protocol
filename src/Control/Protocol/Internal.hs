{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- |
-- Module : Control.Protocol.Internal
-- Copyright : (c) Evgeny Poberezkin
-- License : BSD3
--
-- Maintainer  : evgeny@poberezkin.com
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides type families to combine/extract the individual states of parties
-- as defined by command constructors into/from the combined system/protocol state defined as type-level list.
module Control.Protocol.Internal where

import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | Extracts party from command type parameter.
type family P (partyCmd :: (party, s, s)) where
  P '(p, _, _) = p

-- | Projects the state of one party from the list of states.
type family Prj (parties :: [pk]) (state :: [k]) (party :: pk) :: k where
  Prj (p ': _) (s ': _) p = s
  Prj (_ ': ps) (_ ': ss) p = Prj ps ss p
  Prj '[] _ p = TypeError (NoParty p)
  Prj _ '[] p = TypeError (NoParty p :$$: StateError)

-- | Injects the state of some party into the party's position in the list of states.
type family Inj (parties :: [pk]) (state :: [k]) (p :: pk) (s' :: k) :: [k] where
  Inj (p ': _) (_ ': ss) p s' = s' ': ss
  Inj (_ ': ps) (s ': ss) p s' = s ': Inj ps ss p s'
  Inj '[] _ p _ = TypeError (NoParty p)
  Inj _ '[] p _ = TypeError (NoParty p :$$: StateError)

type NoParty p = Text "Party " :<>: ShowType p :<>: Text " is not found."

type StateError = Text "Specified fewer protocol states than parties."
