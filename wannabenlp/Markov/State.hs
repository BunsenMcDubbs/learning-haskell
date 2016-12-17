{-# LANGUAGE StandaloneDeriving #-}

module Markov.State
( State (..)
, toState
, fromState
) where

data State s = InitialState | TerminalState | State s
-- This uses GADT... spooky? yes.
deriving instance Eq s => Eq (State s)
deriving instance Ord s => Ord (State s)
deriving instance Show s => Show (State s)

toState :: (Ord s) => s -> State s
toState s = State s

fromState :: State s -> s
fromState (State s) = s
