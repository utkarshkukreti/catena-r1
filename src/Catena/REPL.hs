module Main (
  main
) where

import Catena
import Catena.Evaluator
import Catena.Parser
import System.IO (hFlush, stdout)

showState :: State -> String
showState state = "stack: " ++ show (stack state)

r :: IO String
r = putStr "> " >> hFlush stdout >> getLine

e :: State -> String -> EvalResult
e state line = case parse line of
  Left err     -> Left $ ParseError err
  Right tokens -> eval state { queue = tokens }

p :: State -> EvalResult -> IO State
p oldState (Left err) = print err >> return oldState
p _ (Right newState)  = putStrLn (showState newState) >> return newState

l :: State -> IO ()
l state = do
  line <- r
  case line of
    ""     -> l state
    "exit" -> return ()
    _      -> return (e state line) >>= p state >>= l

repl :: IO ()
repl = l defaultState

main :: IO ()
main = repl
