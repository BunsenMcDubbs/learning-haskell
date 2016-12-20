import System.IO

main = do
    let p = ["import System.IO\n\n","main = do\n","    let p = "]
    let q = "mapM_ putStr (p ++ [show p, \"\\n    let q = \" ++ show q, \"\\n    \" ++ q, \"\\n\"])"
    mapM_ putStr (p ++ [show p, "\n    let q = " ++ show q, "\n    " ++ q, "\n"])
