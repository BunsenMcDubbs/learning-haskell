{-
main = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name
-}

import Control.Monad
main = do
    line <- getLine
    when (not $ null line) $ do 
        putStrLn $ reverseWords line
        main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
