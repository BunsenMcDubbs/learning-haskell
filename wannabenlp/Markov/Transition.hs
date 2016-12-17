{-# LANGUAGE StandaloneDeriving #-}

module Markov.Transition
( Transition (..)
, toTransition
, fromTransition
) where

data Transition t = Initialize | Terminate | Transition t
deriving instance Eq t => Eq (Transition t)
deriving instance Ord t => Ord (Transition t)
deriving instance Show t => Show (Transition t)

toTransition :: (Ord t) => t -> Transition t
toTransition t = Transition t

fromTransition :: Transition t -> t
fromTransition (Transition t) = t
