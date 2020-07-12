{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- |
-- Module : Control.Protocol
-- Copyright : (c) Evgeny Poberezkin
-- License : BSD3
--
-- Maintainer  : evgeny@poberezkin.com
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides type 'Protocol' to model distributed multi-party protocols,
-- ensuring the continuity of the associated resource state transitions on the type level
-- for all protocol commands and scenarios.
--
-- It accepts used-defined data type for protocol commands (GADT of kind 'Command') and
-- the list of protocol participants to create protocol type.
--
-- Command type serves as a single abstract api between all participants - it is defined
-- to allow decoupling type-level requirements for participants implementations.
--
-- Function '(->:)' wraps commands into free parameterized monad (see <https://hackage.haskell.org/package/freer-indexed freer-indexed>)
-- so that they can be chained into type-aligned scenarios, optionally written using do notation.
-- These scenarios can be interpreted in any monad using 'runProtocol' function,
-- e.g. to print protocol description or diagram or to execute system-level integration tests.
--
-- See protocol definition and scenario examples in @<./Control-Protocol-Example.html Control.Protocol.Example>@.
module Control.Protocol
  ( Command,
    Protocol,
    ProtocolCmd,
    (->:),
    runProtocol,
  )
where

import Control.Protocol.Internal
import Control.XFreer
import Data.Kind
import Data.Singletons

-- | Defines the kind of the command data type used by 'Protocol':
--
--     * @party@ - the type (normally enumerable) that defines parties of the protocol (here it is used as a kind). This type should be singletonized.
--     * @state@ - the kind of the protocol resource state.
--
-- The first type-level tuple in command constructors defines the party that sends the command, with the initial and final resource states for that party.
--
-- The second tuple defines the party that executes command (e.g., provides some api) with its initial final and states.
--
-- See @<./Control-Protocol-Example.html Control.Protocol.Example>@ for command example.
type Command party state = (party, state, state) -> (party, state, state) -> Type -> Type

-- | Protocol data type that wraps a command and explicitly adds command participants.
--
-- Its only constructor that is not exported adds command participants
-- and combines participant states in type-level list so that they can be chained
-- in type-aligned sequence of commands:
--
-- > ProtocolCmd ::
-- >   Sing (from :: p) ->
-- >   Sing (to :: p) ->
-- >   cmd '(from, Prj ps s from, fs') '(to, Prj ps s to, ts') a ->
-- >   ProtocolCmd cmd ps s (Inj ps (Inj ps s from fs') to ts') a
--
-- Here:
--
--     * @from@ - type of party that sends the command.
--     * @to@ - type of party that executes command (e.g., provides some API).
--
-- 'Protocol' type synonym should be used to construct this type, and function '(->:)' should be used instead of the constructor.
data ProtocolCmd (cmd :: Command p k) (parties :: [p]) (s :: [k]) (s' :: [k]) (a :: Type) where
  ProtocolCmd ::
    Sing (from :: p) ->
    Sing (to :: p) ->
    cmd '(from, Prj ps s from, fs') '(to, Prj ps s to, ts') a ->
    ProtocolCmd cmd ps s (Inj ps (Inj ps s from fs') to ts') a

-- | Type synonym to create protocol data type ('ProtocolCmd' wrapped in 'XFree' - parameterized free monad):
--
--     * @cmd@ - user-defined command type constructor that should have the kind 'Command'.
--     * @parties@ - type-level list of participants - it defines the order of participant states in the combined state of the system described by the protocol.
type Protocol cmd parties = XFree (ProtocolCmd cmd parties)

infix 6 ->:

-- | Function that wraps command into 'ProtocolCmd' type converted into free parameterized monad.
(->:) ::
  -- | party that sends command
  Sing from ->
  -- | party that executes command
  Sing to ->
  -- | command - its initial states for both parties are projected from system state
  cmd '(from, Prj ps s from, fs') '(to, Prj ps s to, ts') a ->
  -- | final protocol state injects final states of both participants
  Protocol cmd ps s (Inj ps (Inj ps s from fs') to ts') a
(->:) f t c = xfree $ ProtocolCmd f t c

-- | 'runProtocol' interprets protocol scenario in any monad,
-- using passed 'runCmd' function that interprets individual commands.
runProtocol ::
  forall m cmd ps s s' a.
  Monad m =>
  -- | function to interpret a command
  (forall from to b. (Sing (P from) -> Sing (P to) -> cmd from to b -> m b)) ->
  -- | protocol scenario (see example in @'Control.Protocol.Example.Scenario'@)
  Protocol cmd ps s s' a ->
  m a
runProtocol runCmd = loop
  where
    loop :: forall s1 s2 b. Protocol cmd ps s1 s2 b -> m b
    loop (Pure x) = return x
    loop (Bind c f) = run c >>= loop . f
    run :: forall s1 s2 b. ProtocolCmd cmd ps s1 s2 b -> m b
    run (ProtocolCmd from to cmd) = runCmd from to cmd
