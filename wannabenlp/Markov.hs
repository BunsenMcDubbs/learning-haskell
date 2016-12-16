{-# LANGUAGE StandaloneDeriving #-}
module Markov
( MarkovChain
, State
, Transition
, toState
, fromState
, toTransition
, fromTransition
, empty
, observe
, initialize
, terminate
, transitionFreqs
, transitionCounts
, transition
) where

import qualified Data.Map.Strict as Map

{-|
 - Markov Chain represented by a map of maps relating states to observed
 - transitions and the number of observations for each occurance.
 -}
data MarkovChain s t =
    MarkovChain
        (Map.Map (State s) (Map.Map (Transition t) Integer))
        (Map.Map (State s) Integer)
    deriving (Show)

data State s = InitialState | TerminalState | State s
-- This uses GADT... spooky? yes.
deriving instance Eq s => Eq (State s)
deriving instance Ord s => Ord (State s)
deriving instance Show s => Show (State s)

data Transition t = Initialize | Terminate | Transition t
deriving instance Eq t => Eq (Transition t)
deriving instance Ord t => Ord (Transition t)
deriving instance Show t => Show (Transition t)

toState :: (Ord s) => s -> State s
toState s = State s

fromState :: State s -> s
fromState (State s) = s

toTransition :: (Ord t) => t -> Transition t
toTransition t = Transition t

fromTransition :: Transition t -> t
fromTransition (Transition t) = t

{-|
 - Constructs an empty MarkovChain
 -}
empty :: MarkovChain s t
empty = MarkovChain (Map.empty) (Map.empty)

{-|
 - Add an observation to a MarkovChain consisting of an old state and a
 - transition
 -}
observe :: (Ord s, Ord t) =>
    State s -> Transition t -> MarkovChain s t -> MarkovChain s t
observe state@(State _) Initialize (MarkovChain chain initialStates) =
    MarkovChain
        (chain)
        (Map.insert state (obs + 1) initialStates)
    where
        obs = Map.findWithDefault 0 state initialStates
observe state@(State _) tr (MarkovChain chain initialStates) =
    MarkovChain
        (Map.insert state (Map.insert tr (s_t_obs + 1) s_obs) chain)
        (initialStates)
    where
        s_obs = Map.findWithDefault (Map.singleton tr 0) state chain 
        s_t_obs = Map.findWithDefault 0 tr s_obs

{-|
 - Add an initial state. Equivalent to `observe state Initialize mc`
 -}
initialize :: (Ord s, Ord t) => State s -> MarkovChain s t -> MarkovChain s t
initialize state@(State _) mc = observe state Initialize mc

{-|
 - Add a terminating transition. Equivalent to `observe state Terminate mc`
 -}
terminate :: (Ord s, Ord t) => State s -> MarkovChain s t -> MarkovChain s t
terminate state@(State _) mc = observe state Terminate mc

{-|
 - Get a list of transactions and their frequencies for a given state
 -}
transitionFreqs ::
    (Ord s) => State s -> MarkovChain s t -> [(Transition t, Double)]
transitionFreqs TerminalState _ = []
transitionFreqs state mc@(MarkovChain chain _) = 
    map
        (\(t, i) -> (t, (fromIntegral i) / total))
        counts
    where
        counts = transitionCounts state mc
        total = fromIntegral . sum . map (\(t, c) -> c) $ counts

{-|
 - Get a list of transactions and their observation counts for a given state
 -}
transitionCounts ::
    (Ord s) => State s -> MarkovChain s t -> [(Transition t, Integer)]
transitionCounts TerminalState _ = []
transitionCounts state (MarkovChain chain _) =
    Map.toList $ Map.findWithDefault Map.empty state chain

{-|
 - Randomly select a transition for a given state using observed frequencies
 -}
transition ::
    (Ord s) => State s -> MarkovChain s t -> Transition t
transition state mc = undefined
