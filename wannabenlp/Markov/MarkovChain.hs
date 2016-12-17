module Markov.MarkovChain
( MarkovChain (..)
, empty
, observe
, initialize
, terminate
, transition
, initialStateFreqs
, initialStateCounts
, getInitialState
, transitionFreqs
, transitionCounts
, getTransition
, walk
) where

import System.Random
import qualified Data.Map.Strict as Map

import Markov.State
import Markov.Transition

{-|
 - Markov Chain represented by a map of maps relating states to observed
 - transitions and the number of observations for each occurance.
 -}
data MarkovChain s t =
    MarkovChain
        (Map.Map (State s) (Map.Map (Transition t) Int))
        (Map.Map (State s) Int)
    deriving (Show)

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
 - Add an initial state. Equivalent to `observe (toState state) Initialize mc`
 -}
initialize :: (Ord s, Ord t) => s -> MarkovChain s t -> MarkovChain s t
initialize s mc = observe (toState s) Initialize mc

{-|
 - Add a terminal state. Equivalent to `observe (toState state) Terminate mc`
 -}
terminate :: (Ord s, Ord t) => s -> MarkovChain s t -> MarkovChain s t
terminate s mc = observe (toState s) Terminate mc

{-|
 - Add a transition.
 - Equivalent to `observe (toState state) (toTransition transition) mc`
 -}
transition :: (Ord s, Ord t) => s -> t -> MarkovChain s t -> MarkovChain s t
transition s t mc = observe (toState s) (toTransition t) mc

{-|
 - Get a list of initial states and their frequencies
 -}
initialStateFreqs :: MarkovChain s t -> [(State s, Double)]
initialStateFreqs mc = 
    map (\(s, c) -> (s, (fromIntegral c) / total)) counts
    where
        counts = initialStateCounts mc
        total = fromIntegral . sum . map (\(t, c) -> c) $ counts

{-|
 - Get a list of initial states and their observation counts
 -}
initialStateCounts :: MarkovChain s t -> [(State s, Int)]
initialStateCounts (MarkovChain _ initialStates) = Map.toList initialStates 

{-|
 - Randomly select an initial state using observed frequencies
 -}
getInitialState :: (Ord s) => StdGen -> MarkovChain s t -> (State s, StdGen)
getInitialState randGen mc = (state, randGen')
    where
        sCounts = initialStateCounts mc
        numStates = sum . map (\(s, count) -> count) $ sCounts
        (index, randGen') = randomR (0, numStates - 1) randGen
        state = (concat $ map (\(s, cnt) -> replicate cnt s) sCounts) !! index

{-|
 - Get a list of transactions and their frequencies for a given state
 -}
transitionFreqs ::
    (Ord s) => State s -> MarkovChain s t -> [(Transition t, Double)]
transitionFreqs state mc@(MarkovChain chain _) = 
    map
        (\(t, c) -> (t, (fromIntegral c) / total))
        counts
    where
        counts = transitionCounts state mc
        total = fromIntegral . sum . map (\(t, c) -> c) $ counts

{-|
 - Get a list of transactions and their observation counts for a given state
 -}
transitionCounts ::
    (Ord s) => State s -> MarkovChain s t -> [(Transition t, Int)]
transitionCounts TerminalState _ = []
transitionCounts state (MarkovChain chain _) =
    Map.toList $ Map.findWithDefault Map.empty state chain


{-|
 - Randomly select a transition for a given state using observed frequencies
 -}
getTransition ::
    (Ord s) => StdGen -> State s -> MarkovChain s t -> (Transition t, StdGen)
getTransition randGen InitialState _ = (Initialize, randGen)
getTransition randGen TerminalState _ = (Terminate, randGen)
getTransition randGen state mc = (transition, randGen')
    where
        trCounts = transitionCounts state mc
        numTrs = sum . map (\(tr, count) -> count) $ trCounts
        (index, randGen') = randomR (0, numTrs - 1) randGen
        transition = 
            (concat $ map (\(tr, cnt) -> replicate cnt tr) trCounts) !! index

{-!
 - Generate a random walk down the Markov Chain
 -}
walk :: (Ord s) =>
    StdGen ->
    (State s -> Transition t -> State s) ->
    MarkovChain s t ->
    [State s]
walk randGen act mc = walkHelper randGen InitialState wrappedAct mc
    where wrappedAct = walkActWrapper act

walkHelper :: (Ord s) =>
    StdGen ->
    State s ->
    (State s -> Transition t -> State s) ->
    MarkovChain s t ->
    [State s]
walkHelper _ TerminalState _ _ = [TerminalState]
walkHelper randGen state act mc =
    (state) : (walkHelper randGen' nextState act mc)
    where 
        (nextState, randGen') = case state of
            InitialState -> getInitialState randGen mc
            (State _) ->
                let (transition, randGen'') = getTransition randGen state mc
                in (act state transition, randGen'')

walkActWrapper ::
    (State s -> Transition t -> State s) ->
    State s ->
    Transition t ->
    State s
walkActWrapper _ _ Terminate = TerminalState
walkActWrapper act s t = act s t

