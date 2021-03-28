module Parse where

import Text.ParserCombinators.ReadP
import Types

parse :: String -> [(FiniteAutomaton, String)]
parse = readP_to_S parseFiniteAutomaton

--parse = readP_to_S parse_FiniteAutomaton
--parse = readP_to_S parse_FiniteAutomaton "1,2,3,4,5,6,11\nab\n1\n1,6\n1,a,6\n1,b,2\n2,a,5"
--parse = fst . minimumBy (comparing snd) . readP_to_S parse_FiniteAutomaton
--parse = last . readP_to_S parse_FiniteAutomaton

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

statesParser :: ReadP [[Char]]
statesParser = sepBy stateParse separator

isStateSymbols :: Char -> Bool
isStateSymbols  character = character `elem` "0123456789"

stateParse :: ReadP [Char]
stateParse = many1 (satisfy isStateSymbols)

-------------------------------------------------------------

alphabetParser :: ReadP [Char]
alphabetParser = many alphabetSymbolParse -- todo many/many1

isAlphabetSymbol :: Char -> Bool
isAlphabetSymbol character = character `elem` "abcdefghijklmnopqrstuvwxyz"

alphabetSymbolParse :: ReadP Char
alphabetSymbolParse = satisfy isAlphabetSymbol

-------------------------------------------------------------

startStateParser :: ReadP [Char]
startStateParser = stateParse

-------------------------------------------------------------

acceptStatesParser :: ReadP [[Char]]
acceptStatesParser = statesParser

-------------------------------------------------------------

transitionsParser :: ReadP [Transition]
transitionsParser = sepBy transitionParser skipSpaces

transitionParser :: ReadP Transition
transitionParser = Transition
  <$> stateParse <* string ","
  <*> alphabetSymbolParse <* string ","
  <*> stateParse <* skipSpaces

-------------------------------------------------------------

allUniqueElements :: (Eq a) => [a] -> Bool
allUniqueElements []     = True
allUniqueElements (x:xs) = notElem x xs && allUniqueElements xs

subSequence :: Eq a => [a] -> [a] -> Bool
subSequence [] _ = True
subSequence (_:_ ) [] = False
subSequence (a:as) (b:bs) = (if a == b then as else a:as) `subSequence` bs

validateTransitions :: FiniteAutomaton -> Bool
validateTransitions automaton_a = all (`validateTransition` automaton_a) (transition_function automaton_a)

validateTransition :: Transition -> FiniteAutomaton -> Bool
validateTransition transition automaton_a =
    elem state_a (states automaton_a) &&
    elem sym (alphabet automaton_a) &&
    elem state_b (states automaton_a)
      where (state_a,sym,state_b) = (source transition, symbol transition, destination transition)

validateAutomatonA :: FiniteAutomaton -> Bool
validateAutomatonA automaton_a@(FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transition_function_a) =
  is_valid
  where
    is_valid =
      elem start_state_a states_a &&
      allUniqueElements states_a &&
      allUniqueElements alphabet_a &&
      subSequence accept_states_a states_a &&
      allUniqueElements transition_function_a &&
      validateTransitions automaton_a