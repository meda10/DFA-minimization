--  Project: FLP - Functional - DKA-MKA
--  Author: Petr Medek 
--  Year: 2021

module Parse where

import Text.ParserCombinators.ReadP
import Types
import Data.List

parse :: String -> [(FiniteAutomaton, String)]
parse = readP_to_S parseFiniteAutomaton

--Sort alphabet and states in automaton 
sortItemsInAutomaton :: FiniteAutomaton -> FiniteAutomaton
sortItemsInAutomaton fin_a@(FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transitions_a) =
  fin_a { states = sort states_a,
                   alphabet = sort alphabet_a,
                   start_state = start_state_a,
                   accept_states = sort accept_states_a,
                   transitions = sort transitions_a}

--Parse automaton 
parseFiniteAutomaton :: ReadP FiniteAutomaton
parseFiniteAutomaton = FiniteAutomaton
  <$> statesParser <* eol
  <*> alphabetParser <* eol
  <*> startStateParser <* eol
  <*> acceptStatesParser <* eol
  <*> transitionsParser <* eof

eol :: ReadP String
eol = string "\r" <++ string "\r\n" <++ string "\n" <++ string "\r"

separator :: ReadP Char
separator = char ','

-------------------------------------------------------------

--Parse states 
statesParser :: ReadP [[Char]]
statesParser = sepBy stateParse separator

isStateSymbols :: Char -> Bool
isStateSymbols  character = character `elem` "0123456789"

stateParse :: ReadP [Char]
stateParse = many1 (satisfy isStateSymbols)

-------------------------------------------------------------

--Parse Alphabet 
alphabetParser :: ReadP [Char]
alphabetParser = many alphabetSymbolParse -- todo many/many1

isAlphabetSymbol :: Char -> Bool
isAlphabetSymbol character = character `elem` "abcdefghijklmnopqrstuvwxyz"

alphabetSymbolParse :: ReadP Char
alphabetSymbolParse = satisfy isAlphabetSymbol

-------------------------------------------------------------

--Parse Start state 
startStateParser :: ReadP [Char]
startStateParser = stateParse

-------------------------------------------------------------

--Parse accept states 
acceptStatesParser :: ReadP [[Char]]
acceptStatesParser = statesParser

-------------------------------------------------------------

--Parse transitions 
transitionsParser :: ReadP [Transition]
transitionsParser = sepBy transitionParser skipSpaces

transitionParser :: ReadP Transition
transitionParser = Transition
  <$> stateParse <* string ","
  <*> alphabetSymbolParse <* string ","
  <*> stateParse <* skipSpaces

-------------------------------------------------------------

allUniqueElements :: (Eq a) => [a] -> Bool
allUniqueElements [] = True
allUniqueElements (x:xs) = notElem x xs && allUniqueElements xs

isSubset :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
isSubset a b = all (`elem` b) a

validateTransitions :: FiniteAutomaton -> Bool
validateTransitions automaton_a = all (`validateTransition` automaton_a) (transitions automaton_a)

validateTransition :: Transition -> FiniteAutomaton -> Bool
validateTransition transition automaton_a =
    elem state_a (states automaton_a) &&
    elem sym (alphabet automaton_a) &&
    elem state_b (states automaton_a)
      where (state_a,sym,state_b) = (source transition, symbol transition, destination transition)

--Check if automaton is valid
validateAutomatonA :: FiniteAutomaton -> Bool
validateAutomatonA automaton_a@(FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transitions_a) =
  is_valid
  where
    is_valid =
      elem start_state_a states_a &&
      allUniqueElements states_a &&
      allUniqueElements alphabet_a &&
      accept_states_a `isSubset` states_a &&
      allUniqueElements transitions_a &&
      validateTransitions automaton_a