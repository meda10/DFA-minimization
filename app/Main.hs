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
    action $ sortItemsInAutomaton (fst (head parsed_input))

--    let automaton = sortItemsInAutomaton (fst (head parsed_input))
--    print "------RM------"
--    let automat = removeUnusedStates $ createSinkState automaton
--    print automat
--    let a =  minimalAutomaton automat
--    print "------MIN------"
--    print a
--
--    let p = [accept_states automat, states automat \\ accept_states automat]
----    let new_states = sortStates (mainWhile p p (alphabet automat) (transitions automat))
--    let new_states =  mainWhile p p (alphabet automat) (transitions automat)
--    print "----- New st ------"
--    print new_states
--
--    let new_start_state = createStartState new_states (start_state automat)
--    print "----- New start st ------"
--    print new_start_state
--
----    let renamed_states = renameStates $ new_start_state : delete new_start_state new_states
----    print "----- Renamed st ------"
----    print renamed_states
----
----    let new_transitions = createNewTransitions new_states (transitions automat)
--    let nnn_state_names = getStateNames (states automat) new_states
--    let new_transitions = nub (renameTransitions (states automat) nnn_state_names (transitions automat))
--    print "----- New tr ------"
--    print (length new_transitions)
--    print new_transitions
--
----    let renamed_tr = renameTransitions new_states renamed_states new_transitions
----    print "----- Renamed tr ------"
----    print renamed_tr
--
--    let new_accept_states = createFinalStates (accept_states automat) new_states
--    print "----- New accept ------"
----    print new_states
----    print (accept_states automat)
--    print new_accept_states
--
----    let renamed_fin = renameFinalStates new_states renamed_states new_accept_states
----    print "----- Renamed accept ------"
----    print renamed_fin
--
--    print "----- Rename From To ------"
--    let renamed_start_state = renameStartStateInStates new_states new_start_state
--    print new_states
--    print renamed_start_state
--    print "----- Rename Done ------"
--    let (s, st, fin, tr) = renameAll new_states renamed_start_state new_start_state new_accept_states new_transitions
--    print (s, st, fin, tr)
--
--    print "----- Rename From To ------"
--    let yyy_new = getIndex s (getStatesPositionsInTransition tr)
--    print s
--    print yyy_new
--    print "----- Rename Done ------"
--    let (s1, st1, fin1, tr1) = renameAll s yyy_new st fin tr
--    print (s1, st1, fin1, tr1)


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


validateParsedInput :: [(FiniteAutomaton, b)] -> IO Bool
validateParsedInput [] = exitWithError InvalidFileFormat 10
validateParsedInput parsed_input = return (validateAutomatonA (sortItemsInAutomaton (fst (head parsed_input))))


parseArgs :: [Char] -> b -> IO (FiniteAutomaton -> IO (), b)
parseArgs arguments input = case arguments of
  "-i" -> return (printFiniteAutomaton, input)
  "-t" -> return (printMinimalFiniteAutomaton, input)
  "-m" -> return (printWellDefinedFiniteAutomaton, input)
  _ -> exitWithError InvalidArguments 10

printFiniteAutomaton :: FiniteAutomaton -> IO ()
printFiniteAutomaton = putStr . show

printMinimalFiniteAutomaton :: FiniteAutomaton -> IO ()
printMinimalFiniteAutomaton automaton = printFiniteAutomaton $ minimalAutomaton (removeUnusedStates $ createSinkState automaton)

printWellDefinedFiniteAutomaton :: FiniteAutomaton -> IO ()
printWellDefinedFiniteAutomaton automaton = printFiniteAutomaton (removeUnusedStates $ createSinkState automaton)

exitWithError :: Show a => a -> Int -> IO b
exitWithError error_msg code = hPutStrLn stderr ("ERROR: " ++ show error_msg) >> exitWith (ExitFailure code)
--exitWithError :: String -> Int -> IO a
--exitWithError str code = hPutStrLn stderr str >> exitWith (ExitFailure code)
