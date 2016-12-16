module ParseText
( fromSentence
, fromSentences
, fromFile
) where

import System.Environment
import System.IO
import Data.Char
import Data.List.Split
import Markov

fromSentence :: [String] -> MarkovChain String String -> MarkovChain String String
fromSentence [] mc = mc
fromSentence (w1:[]) mc = Markov.observe (toState w1) terminateChain mc
fromSentence (w1:w2:ws) mc = Markov.observe (toState w1) (toTransition w2) $ fromSentence (w2:ws) mc

fromSentences :: [String] -> MarkovChain String String
fromSentences sentences =
    foldr (fromSentence) chain $ map words sentences
    where
        chain = Markov.empty -- TODO need to initialize state

fromFile :: String -> IO (MarkovChain String String)
fromFile fileName = do
    contents <- readFile fileName
    let sentences = splitOneOf ".!?\n" $ map toLower $ contents
        markovChain = fromSentences sentences
    return markovChain

main = do
    (fileName:[]) <- getArgs
    markovChain <- fromFile fileName
    print markovChain

