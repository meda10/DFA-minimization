module Parse where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Text.Read (readPrec)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (readPrec_to_P, minPrec)

import Types

--seznam vsech stavu
--abeceda
--poćatecńi stav
--seznam koncovych stavu
--pravidlo 1
--pravidlo n


--parse :: ReadP Finite_automaton
--parse = do
--    states <- null
--    alphabet <- alphabet_parser
--    start_state <- null
--    accept_states <- null
--    transition_function <- null
--    return (Finite_automaton states alphabet transition_function start_state accept_states)


fin_parse :: ReadS Finite_automaton
fin_parse = readP_to_S parser

--parseRead :: Read a => ReadP a
--parseRead = readPrec_to_P readPrec minPrec

parser :: ReadP Finite_automaton
parser = Finite_automaton
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


-- Parses the starting symbol.
--startingSymbolParser :: Parser Symbol
--startingSymbolParser = oneOf nonterminalSymbols


-- Parses a list of production rules.
--rulesParser :: Parser Rules
--rulesParser = endBy ruleParser newline

-- Parses single production rules.
--ruleParser :: Parser Rule
--ruleParser = (,)
-- <$> oneOf nonterminalSymbols <* string "->"
-- <*> ( count 1 (char epsSymbol)
--   <|> many1 (oneOf $ nonterminalSymbols ++ terminalSymbols) )


--
--input_parse :: [String] -> IO Finite_automaton
--input_parse input = do
--
--    return Finite_automaton {
--
--    }







