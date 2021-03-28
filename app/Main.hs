module Main (main) where

import System.Environment
import System.Exit
import System.IO
import Data.Dynamic
import System.IO.Error
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Char
import Text.Printf

import Types
import Parse

main :: IO ()
main = do
  argv <- getArgs
  (action, input) <- case argv of
    [args] -> parse_args args =<< getContents
    [args, filename] -> parse_args args =<< readFile filename
    _ -> exit_with_error Invalid_arguments 44

--  parsed_input <- parse <$> readFile "app/input.in"
  let parsed_input = parse input
  is_valid <- validate_parsed_input parsed_input
  if is_valid
  then do
    let automaton = (fst (head parsed_input))
    action $ automaton
  else
    exit_with_error Invalid_automaton_format 2

--  do_something (parse input)


--  if parsed_input == []
--  then exit_with_error Invalid_file_format 10
--  else do
--    let automaton = (fst (head parsed_input))
--    let is_valid = validate_automaton_a automaton
----    let is_valid = validate_automaton_a ini
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
--      exit_with_error Invalid_automaton_format 2


validate_parsed_input :: [(Finite_automaton, b)] -> IO Bool
validate_parsed_input [] = exit_with_error Invalid_file_format 10
validate_parsed_input parsed_input = return (validate_automaton_a (fst (head parsed_input)))


parse_args :: [Char] -> b -> IO (Finite_automaton -> IO (), b)
parse_args arguments input = case arguments of
  "-i" -> return (print_finite_automaton, input)
  "-t" -> return (print_finite_automaton, input)
  _ -> exit_with_error Invalid_arguments 10


print_finite_automaton :: Finite_automaton -> IO ()
print_finite_automaton = putStr . show


-- Prints the context-free grammar to the standard output after the 'simplify1'
-- function has been performed.
--printCFGSimplify1 :: CFG -> IO ()
--printCFGSimplify1 = printCFG . simplify1


-- Terminates the program with an error code and an error message.
--undefined_option :: String -> IO a
--undefined_option str = hPutStrLn stderr str >> exitFailure

exit_with_error :: Show a => a -> Int -> IO b
exit_with_error error_msg code = hPutStrLn stderr ("ERROR: " ++ show error_msg) >> exitWith (ExitFailure code)
--exit_with_error :: String -> Int -> IO a
--exit_with_error str code = hPutStrLn stderr str >> exitWith (ExitFailure code)


