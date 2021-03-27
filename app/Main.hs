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

  putStrLn "The arguments are:"
  mapM putStrLn argv

  (action, input) <- case argv of
    [args] -> parse_args args =<< getContents
    [args, filename] -> parse_args args =<< readFile filename
    _ -> exit_with_error "Excepting an option argument and optionally an input" 44

--  putStrLn action
--  putStrLn input
  putStrLn "------------------"
  print (dynTypeRep (toDyn action))
  print (dynTypeRep (toDyn input))
  putStrLn "------------------"

  ini <- parse <$> readFile "app/input.in"
--  print ini
  let is_valid = validate_automaton_a ini
  print (dynTypeRep (toDyn ini))
  print (dynTypeRep (toDyn is_valid))
  putStrLn "------------------"
--  print ini
--  print is_valid
  if True then print is_valid else exit_with_error Invalid_file_format 2

  putStrLn "------------------"
  action $ ini


parse_args :: [Char] -> b -> IO (Finite_automaton -> IO (), b)
parse_args arguments input = case arguments of
  "-i" -> return (print_finite_automaton, input)
  "-t" -> return (print_finite_automaton, input)
  _ -> exit_with_error ("Unknown option: " ++ arguments) 10


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


