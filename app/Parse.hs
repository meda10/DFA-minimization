module Parse where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Text.ParserCombinators.ReadP

import Types


parse :: ReadS Finite_automaton
parse = readP_to_S parse_finite_automaton

parse_finite_automaton :: ReadP Finite_automaton
parse_finite_automaton = Finite_automaton
  <$> states_parser <* eol
  <*> alphabet_parser <* eol
  <*> start_state_parser <* eol
  <*> accept_states_parser <* eol
  <*> transitions_parser <* eof

eol :: ReadP String
eol = (string "\n\r")
    <++ (string "\r\n")
    <++ (string "\n")
    <++ (string "\r")

separator :: ReadP Char
separator = char ','

-------------------------------------------------------------

states_parser :: ReadP [[Char]]
states_parser = sepBy state_parse separator

is_state_symbols :: Char -> Bool
is_state_symbols  char = any (char ==) "123456789"

state_parse :: ReadP [Char]
state_parse = many1 (satisfy is_state_symbols)

-------------------------------------------------------------

alphabet_parser :: ReadP [Char]
alphabet_parser = many1 alphabet_symbol_parse

is_alphabet_symbol :: Char -> Bool
is_alphabet_symbol char = any (char ==) "abcdefghijklmnopqrstuvwxyz"

alphabet_symbol_parse :: ReadP Char
alphabet_symbol_parse = satisfy is_alphabet_symbol

-------------------------------------------------------------

start_state_parser :: ReadP [Char]
start_state_parser = state_parse

-------------------------------------------------------------

accept_states_parser :: ReadP [[Char]]
accept_states_parser = states_parser

-------------------------------------------------------------

transitions_parser :: ReadP [Transition]
transitions_parser = sepBy transition_parser skipSpaces

transition_parser :: ReadP Transition
transition_parser = Transition
  <$> state_parse <* string ","
  <*> alphabet_symbol_parse <* string ","
  <*> state_parse <* skipSpaces

-------------------------------------------------------------

--input_parse :: [String] -> IO Finite_automaton
--input_parse input = do
--
--    return Finite_automaton {
--
--    }







