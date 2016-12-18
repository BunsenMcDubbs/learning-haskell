import NSpell

import System.IO
import Control.Monad
import qualified Data.Text as T

main = do
    contents <- readFile "big.txt"
    let ws = (map T.unpack) . T.words . T.toLower . T.pack $ contents
        wc = foldl (observe) (empty)  ws
    forever $ do
        w <- getLine
        putStrLn $ w ++ " -> " ++ correction wc w
