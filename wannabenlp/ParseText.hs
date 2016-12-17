{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-
module ParseText
( fromSentence
, fromSentences
, fromFile
, main
) where
-}

import System.Environment
import System.IO
import System.Random
import Control.Monad
import Data.List.Split
import Data.Char
import qualified Data.Text as T

import Markov.MarkovChain
import Markov.State
import Markov.Transition

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
    let notEmpty = (0 < ) . length
        tNotEmpty = (0 < ) . T.length
        isTerminatingChar = flip elem (".?!" :: String)
        isValidChar c = isAlpha c || isTerminatingChar c || isSpace c
        cleanedContents = T.toLower $ T.filter isValidChar $ T.pack contents
        sentences = filter notEmpty $
            map (words . T.unpack) $
                filter tNotEmpty $
                    T.split isTerminatingChar $ cleanedContents
        markovChain = fromSentences sentences
    return markovChain

act :: State String -> Transition String -> State String
act _ = toState . fromTransition

getRandomSentence :: StdGen -> MarkovChain String String -> String
getRandomSentence randGen mc = showStringStates states
    where states = walk randGen act mc

showStringStates :: [State String] -> String
showStringStates stateList = unwords $ map
    (\state -> case state of
        (State s) -> s
        _ -> "")
    stateList
    
main = do
    (fileName:[]) <- getArgs
    markovChain <- fromFile fileName
    putStrLn $ "Creating 1-Gram Markov Chain from " ++ fileName ++ "..."
    forever $ do
        randGen <- newStdGen
        let sentence = getRandomSentence randGen markovChain
        print sentence
        putStrLn "Press ENTER to continue..."
        l <- getLine
        return sentence
