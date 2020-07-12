{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- |
-- Module : Control.Protocol.Example
-- Copyright : (c) Evgeny Poberezkin
-- License : BSD3
--
-- Maintainer  : evgeny@poberezkin.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Example command and protocol definition.
module Control.Protocol.Example
  ( MyProtocol,
    MyCommand (..),
    scenario,
    Party (..),
    ChannelState (..),
  )
where

import Control.Protocol
import Control.XMonad.Do
import Data.Singletons.TH
import Prelude hiding ((>>), (>>=))

$( singletons
     [d|
       data Party = Recipient | Broker | Sender
         deriving (Show, Eq)
       |]
 )

-- | Example type defining the possible resource states, in this case some communication channel.
data ChannelState
  = None
  | Ready
  | Busy
  deriving (Show, Eq)

-- | Example command data type of kind @'Command' 'Party' 'ChannelState'@.
data MyCommand :: Command Party ChannelState where
  Create :: MyCommand '(Recipient, None, Ready) '(Broker, None, Ready) ()
  Notify :: MyCommand '(Recipient, Ready, Ready) '(Sender, None, Ready) ()
  Send :: String -> MyCommand '(Sender, Ready, Ready) '(Broker, Ready, Busy) ()
  Forward :: MyCommand '(Broker, Busy, Ready) '(Recipient, Ready, Ready) String

-- | Example protocol type.
type MyProtocol = Protocol MyCommand '[Recipient, Broker, Sender]

r :: Sing Recipient
r = SRecipient

b :: Sing Broker
b = SBroker

s :: Sing Sender
s = SSender

-- | Example protocol scenario.
--
-- If you modify this scenario to 'Send' before channel is 'Create'd or
-- to 'Send' two messages in a row without forwarding them, the scenario will not compile.
scenario :: String -> MyProtocol '[None, None, None] '[Ready, Ready, Ready] String
scenario str = do
  r ->: b $ Create
  r ->: s $ Notify
  s ->: b $ Send str
  -- s ->: b $ Send str
  b ->: r $ Forward
