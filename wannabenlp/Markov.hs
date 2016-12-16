{-# LANGUAGE StandaloneDeriving #-}
import qualified Data.Map.Strict as Map

{-
 - Markov Chain represented by a map of maps relating states to observed
 - transitions and the number of observations for each occurance.
 -}

data State s = InitialState | TerminalState | State s
deriving instance Eq s => Eq (State s)
deriving instance Ord s => Ord (State s)
deriving instance Show s => Show (State s)

data Transition t = Initialize | Terminate | Transition t
deriving instance Eq t => Eq (Transition t)
deriving instance Ord t => Ord (Transition t)
deriving instance Show t => Show (Transition t)

data MarkovChain s t =
    MarkovChain
        (Map.Map (State s) (Map.Map (Transition t) Integer))
        (Map.Map (State s) Integer)
    deriving (Show)

empty :: MarkovChain s t
empty = MarkovChain (Map.empty) (Map.empty)

observe :: (Ord s, Ord t) =>
    State s -> Transition t -> MarkovChain s t -> MarkovChain s t
observe state@(State _) Initialize (MarkovChain chain initialStates) =
    MarkovChain
        (chain)
        (Map.insert state (obs + 1) initialStates)
    where
        obs = Map.findWithDefault 0 state initialStates
observe state@(State _) trans@(Transition _) (MarkovChain chain initialStates) =
    MarkovChain
        (Map.insert state (Map.insert trans (s_t_obs + 1) s_obs) chain)
        (initialStates)
    where
        s_obs = Map.findWithDefault (Map.singleton trans 0) state chain 
        s_t_obs = Map.findWithDefault 0 trans s_obs
