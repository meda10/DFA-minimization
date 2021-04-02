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
--    let all_tr = allTransitions (states automaton) (alphabet automaton)
--    let exist_tr = existingTransitions (transitions automaton)
--    let missing_tr = missingTransitions all_tr exist_tr
--    print missing_tr


--    action $ removeUnusedStates $ createSinkState automaton
    let automat = removeUnusedStates $ createSinkState automaton
--    let a =  minimalAutomaton automat
--    print a

    let p = [accept_states automat, states automat \\ accept_states automat]
    let new_states = mainWhile p p (alphabet automat) (transitions automat)
    print "------------"
    print new_states


--    print "------For alphabet------"
--    let w = [["2","3","4","5"]]
--    let a = ["1", "6"]
--    let y_list = [["1","6"],["2","3","4","5"]]
--
--    let f = foreachLetterAlphabet p w a (alphabet automat) (transitions automat)
--    print f
--
--    print "------For XY ------"
--    let x = ["3", "4"]
--    let www = forEachXY p [["2","3","4","5"]] x y_list
--    let b = nonEmptyDifference ["2","3","4","5"] x && nonEmptyIntersect x ["2","3","4","5"]
--    print www
--    print b
--    print "------------"
--    print (nonEmptyDifference ["2","3","4","5"] x)
--    print (nonEmptyIntersect x ["2","3","4","5"])
--    print (["2","3","4","5"] \\ x)
--    print (["2","3","4","5"] `intersect` x)
--    print "------------"

--    print "------Do something ------"
--    let y = doSomething start [["2","3","4","5"]] ["3", "4"] ["2","3","4","5"]
--    print y






  else
    exitWithError InvalidAutomatonFormat 2


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


