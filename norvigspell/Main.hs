import NSpell

import System.CPUTime
import System.Environment
import System.IO
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Text.Printf

dispatch :: String -> [String] -> IO ()
dispatch "interactive" = interactive
dispatch "test" = runTests
dispatch c = doesntExist c

doesntExist :: String -> [String] -> IO ()
doesntExist command _ =
    putStrLn $ "The " ++ command ++ " command doesn't exist!"

wcFromFile :: String -> IO WordCount
wcFromFile fileName = do
    contents <- readFile fileName
    let ws = (map T.unpack) . T.words . T.toLower . T.pack $ contents
        wc = foldl' (observe) (empty) ws
    return $! wc

visiblyWCFromFile :: String -> IO WordCount
visiblyWCFromFile fileName = do
    putStrLn "Tabulating word counts from corpus file..."
    wc <- wcFromFile fileName
    putStrLn "Done tabulating corpus file"
    return wc

type Testset = (String, String) -- (Right, Wrong)

tsFromLine :: String -> [Testset]
tsFromLine l = zip (repeat c) (words ws)
    where (c:ws:_) = splitOn ":" l

tssFromFile :: String -> IO [Testset]
tssFromFile fileName = do
    contents <- readFile fileName
    return $! foldr (\l acc -> (tsFromLine l) ++ acc) [] $ lines contents

test :: WordCount -> Testset -> Bool
test wc (ans,wrong) = ans /= correction wc wrong

runTests :: [String] -> IO ()
runTests (corpusFile:testFile:_) = do
    wc <- visiblyWCFromFile corpusFile
    putStrLn "Starting tests..."
    tss <- tssFromFile testFile
    let failures = filter (test wc) tss
        numTotal = length tss
        numFail = length failures
        numCorrect = numTotal - numFail
        pctCorrect =
            (fromIntegral numCorrect) / (fromIntegral numTotal) :: Double
    printf "Done with tests. Correct: %d/%d (%0.2f%%)\n"
        numCorrect numTotal (pctCorrect * 100.0)

interactive :: [String] -> IO ()
interactive (fileName:_) = do
    wc <- visiblyWCFromFile fileName
    putStrLn "Type in a missspelled word!"
    forever $ do
        putStr $ ">> "
        hFlush stdout
        w <- getLine
        putStrLn $ w ++ " -> " ++ correction wc w

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Valid commands are interactive and test"
        (command:argList) -> dispatch command argList
