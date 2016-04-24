module Main where
import qualified EvalApl as E
import qualified ParseApl as P

main = do
    putStr "> "
    line <- getLine
    (putStrLn . show . E.eval . P.parseString) line
    main
