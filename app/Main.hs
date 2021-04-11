--  Project: FLP - Functional - DKA-MKA
--  Author: Petr Medek 
--  Year: 2021

module Main (main) where

import System.Environment
import System.Exit
import System.IO

import Types
import Parse
import Minimize

main :: IO ()
main = do
  argv <- getArgs
  (action, input) <- case argv of
    [args] -> parseArgs args =<< getContents
    [args, filename] -> parseArgs args =<< readFile filename
    _ -> exitWithError InvalidArguments 44

  let parsed_input = parse input
  is_valid <- validateParsedInput parsed_input
  if is_valid
  then do
    action $ sortItemsInAutomaton (fst (head parsed_input))
  else
    exitWithError InvalidAutomatonFormat 2

--Validates input
validateParsedInput :: [(FiniteAutomaton, b)] -> IO Bool
validateParsedInput [] = exitWithError InvalidFileFormat 10
validateParsedInput parsed_input = return (validateAutomatonA (sortItemsInAutomaton (fst (head parsed_input))))

--Parse arguments
parseArgs :: [Char] -> b -> IO (FiniteAutomaton -> IO (), b)
parseArgs arguments input = case arguments of
  "-i" -> return (printFiniteAutomaton, input)
  "-t" -> return (printMinimalFiniteAutomaton, input)
  "-m" -> return (printWellDefinedFiniteAutomaton, input)
  _ -> exitWithError InvalidArguments 10

--Prints Finite automaton
printFiniteAutomaton :: FiniteAutomaton -> IO ()
printFiniteAutomaton = putStr . show

--Prints Minimal Finite automaton
printMinimalFiniteAutomaton :: FiniteAutomaton -> IO ()
printMinimalFiniteAutomaton automaton = printFiniteAutomaton $ minimalAutomaton (removeUnusedStates $ createSinkState automaton)

--Prints Well defined Finite automaton
printWellDefinedFiniteAutomaton :: FiniteAutomaton -> IO ()
printWellDefinedFiniteAutomaton automaton = printFiniteAutomaton (removeUnusedStates $ createSinkState automaton)

--Exit function
exitWithError :: Show a => a -> Int -> IO b
exitWithError error_msg code = hPutStrLn stderr ("ERROR: " ++ show error_msg) >> exitWith (ExitFailure code)