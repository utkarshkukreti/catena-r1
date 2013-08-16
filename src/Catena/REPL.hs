module Catena.REPL (
  main
) where

import Catena.Evaluator
import Catena.Parser
import System.IO (hFlush, stdout)

showState :: State -> String
showState state = "stack: " ++ (show $ stack state)

repl :: State -> IO ()
repl state = do
  putStrLn $ showState state
  putStr "> "
  hFlush stdout
  line <- getLine
  case line of
    ""     -> repl state
    "exit" -> return ()
    _      -> case parse line of
      Left err -> (putStrLn $ "Parse Error: " ++ err) >> repl state
      Right tokens -> case eval state tokens of
        Left err -> putStrLn err >> repl state
        Right newState -> repl newState

main :: IO ()
main = repl defaultState
