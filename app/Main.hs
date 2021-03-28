module Main (main) where

import System.Environment
import System.Exit
import System.IO
import Data.Dynamic
import Data.List
import System.IO.Error
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Char
import Text.Printf

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

--  parsed_input <- parse <$> readFile "app/input.in"
  let parsed_input = parse input
  is_valid <- validateParsedInput parsed_input
  if is_valid
  then do
    let automaton = fst (head parsed_input)
--    action $ automaton
--    print (all_combinations (states automaton) (alphabet automaton))
    let all_tr = allTransitions (states automaton) (alphabet automaton)
    let exist_tr = existingTransitions (transition_function automaton)
    let missing_tr = missingTransitions all_tr exist_tr
    print all_tr
    print exist_tr
    print missing_tr
    let f =  createSinkState automaton
    print f
    
--    print ( missing_tr)
--    print (all_tr \\ [("1",'a',"x"),("1",'b',"x")])
--    print (all_tr \\ exist_tr)

  else
    exitWithError InvalidAutomatonFormat 2

--  do_something (parse input)


--  if parsed_input == []
--  then exitWithError Invalid_file_format 10
--  else do
--    let automaton = (fst (head parsed_input))
--    let is_valid = validateAutomatonA automaton
----    let is_valid = validateAutomatonA ini
----    putStrLn "--------TYPES----------"
----    print (dynTypeRep (toDyn parsed_input))
----    print (dynTypeRep (toDyn automaton))
----    print (dynTypeRep (toDyn is_valid))
----    putStrLn "--------AUTOMATON----------"
----    print automaton
----    putStrLn "--------VALID----------"
----    print is_valid
--    if is_valid
--    then
--        action $ automaton
--    else
--      exitWithError Invalid_automaton_format 2


validateParsedInput :: [(FiniteAutomaton, b)] -> IO Bool
validateParsedInput [] = exitWithError InvalidFileFormat 10
validateParsedInput parsed_input = return (validateAutomatonA (fst (head parsed_input)))


parseArgs :: [Char] -> b -> IO (FiniteAutomaton -> IO (), b)
parseArgs arguments input = case arguments of
  "-i" -> return (printFiniteAutomaton, input)
  "-t" -> return (printFiniteAutomaton, input)
  _ -> exitWithError InvalidArguments 10


printFiniteAutomaton :: FiniteAutomaton -> IO ()
printFiniteAutomaton = putStr . show


-- Prints the context-free grammar to the standard output after the 'simplify1'
-- function has been performed.
--printCFGSimplify1 :: CFG -> IO ()
--printCFGSimplify1 = printCFG . simplify1


-- Terminates the program with an error code and an error message.
--undefined_option :: String -> IO a
--undefined_option str = hPutStrLn stderr str >> exitFailure

exitWithError :: Show a => a -> Int -> IO b
exitWithError error_msg code = hPutStrLn stderr ("ERROR: " ++ show error_msg) >> exitWith (ExitFailure code)
--exitWithError :: String -> Int -> IO a
--exitWithError str code = hPutStrLn stderr str >> exitWith (ExitFailure code)


