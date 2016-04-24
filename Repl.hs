module Repl where
import qualified EvalApl as E
import qualified ParseApl as P

repl = do
    putStr "> "
    line <- getLine
    (putStrLn . show . E.eval . P.parseString) line
    repl
