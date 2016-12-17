{-# LANGUAGE OverloadedStrings #-}
module ParseText
( fromSentence
, fromSentences
, fromFile
) where

import System.Environment
import System.IO
import System.Random
import Data.List.Split
import qualified Data.Text as T

import Markov

fromSentence ::
    [String] -> MarkovChain String String -> MarkovChain String String
fromSentence [] mc = mc
fromSentence (w1:[]) mc = terminate (toState w1) mc
fromSentence (w1:w2:ws) mc =
    observe (toState w1) (toTransition w2) $ fromSentence (w2:ws) mc

fromSentences :: [[String]] -> MarkovChain String String
fromSentences sentences =
    foldr (fromSentence) initialChain sentences
    where
        initialChain = foldr
            (\s mc -> initialize (toState s) mc)
            empty $
            map head sentences

fromFile :: String -> IO (MarkovChain String String)
fromFile fileName = do
    contents <- readFile fileName
    let notEmpty = (0 < ) . T.length
        isTerminatingChar = flip elem (".?!" :: String)
        notSymbol = not . (flip elem ("\'\",:;()" :: String))
        sentences = map
            (words . filter notSymbol . T.unpack) $
            filter
                notEmpty $
                T.split (isTerminatingChar) $
                    T.toLower $
                    T.pack contents
        markovChain = fromSentences sentences
    return markovChain

generateSentence :: StdGen -> MarkovChain String String -> [String]
generateSentence randGen mc = generateSentenceHelper randGen [] mc

generateSentenceHelper ::
    StdGen -> [String] -> MarkovChain String String -> [String]
generateSentenceHelper randGen [] mc =
    [fromState $ nextState initialState $ transition initialState mc]
generateSentenceHelper randGen (w1:ws) mc =
    undefined

nextState :: State s -> Transition t -> State s
nextState oldState transition = undefined

main = do
    (fileName:[]) <- getArgs
    markovChain <- fromFile fileName
    randGen <- getStdGen
    print $ unwords . reverse $ generateSentence randGen markovChain
