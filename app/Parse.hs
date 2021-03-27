module Parse where

import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

import Types

parse :: String -> Finite_automaton
parse = fst . minimumBy (comparing snd) . readP_to_S parse_finite_automaton

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
is_state_symbols  character = any (character ==) "0123456789"

state_parse :: ReadP [Char]
state_parse = many1 (satisfy is_state_symbols)

-------------------------------------------------------------

alphabet_parser :: ReadP [Char]
alphabet_parser = many1 alphabet_symbol_parse

is_alphabet_symbol :: Char -> Bool
is_alphabet_symbol character = any (character ==) "abcdefghijklmnopqrstuvwxyz"

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

all_unique_elements :: (Eq a) => [a] -> Bool
all_unique_elements []     = True
all_unique_elements (x:xs) = notElem x xs && all_unique_elements xs

sub_sequence :: Eq a => [a] -> [a] -> Bool
sub_sequence [] _ = True
sub_sequence (_:_ ) [] = False
sub_sequence (a:as) (b:bs) = (if a == b then as else a:as) `sub_sequence` bs

validate_transitions :: Finite_automaton -> Bool
validate_transitions automaton_a = all (\transition->validate_transition transition automaton_a) (transition_function automaton_a)

validate_transition :: Transition -> Finite_automaton -> Bool
validate_transition transition automaton_a =
    elem state_a (states automaton_a) &&
    elem sym (alphabet automaton_a) &&
    elem state_b (states automaton_a)
      where (state_a,sym,state_b) = (source transition, symbol transition, destination transition)

validate_automaton_a :: Finite_automaton -> Bool
validate_automaton_a automaton_a@(Finite_automaton states_a alphabet_a start_state_a accept_states_a transition_function_a) =
  if is_valid then True else False
  where
    is_valid =
      elem start_state_a states_a &&
      all_unique_elements states_a &&
      all_unique_elements alphabet_a &&
      sub_sequence accept_states_a states_a &&
      all_unique_elements transition_function_a &&
      validate_transitions automaton_a

