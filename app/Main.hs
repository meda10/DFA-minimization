module Main (main) where

import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Types
import Parse
import Test

import Data.Dynamic
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Char
import Text.Printf

main :: IO ()
main = do
  argv <- getArgs
--
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

--  test <- loadTemplateFile "app/input.in"
--  (test, t) <- print_s "aaaee"
--  (print_s input) >>= print

  ini <- fin_parse <$> readFile "app/input.in"
--  ini <- parseInput <$> readFile "app/input.in"
  mapM_ print $ ini

--  (ini, t) <- parseInput <$> readFile "app/input.in"
--  mapM_ print $ ini
--  mapM_ print $ t

--  print test
  putStrLn "Exit"
--  loadTemplateFile input
  --  run_parser input




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

exit_with_error :: String -> Int -> IO a
exit_with_error str e = hPutStrLn stderr str >> exitWith (ExitFailure e)


