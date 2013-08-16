module Catena.REPL (
  main
) where

import Catena.Evaluator
import Catena.Parser
import Data.List (intercalate)
import System.IO (hFlush, stdout)

showToken :: Token -> String
showToken (Integer x) = show x
showToken (String x) = show x
showToken (Atom x) = x
showToken (List xs) = "[" ++ (intercalate ", " (map showToken xs)) ++ "]"

showStack :: [Token] -> String
showStack xs = showToken $ List $ reverse xs

showState :: State -> String
showState state = "stack: " ++ (showStack $ stack state)

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
