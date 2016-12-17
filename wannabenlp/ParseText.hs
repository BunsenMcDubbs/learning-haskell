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
fromSentence (w1:[]) mc = terminate w1 mc
fromSentence (w1:w2:ws) mc =
    transition w1 w2 $ fromSentence (w2:ws) mc

fromSentences :: [[String]] -> MarkovChain String String
fromSentences sentences =
    foldr (fromSentence) initialChain sentences
    where initialChain = foldr initialize empty $ map head sentences

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

act :: State String -> Transition String -> State String
act _ = toState . fromTransition

getRandomSentence :: StdGen -> MarkovChain String String -> String
getRandomSentence randGen mc = unwords $ map showStringState states
    where states = walk randGen act mc


main = do
    (fileName:[]) <- getArgs
    markovChain <- fromFile fileName
    randGen <- getStdGen
    let sentence = walk randGen act markovChain
    print markovChain
