--  Project: FLP - Functional - DKA-MKA
--  Author: Petr Medek 
--  Year: 2021

module Types where

import Data.List (intercalate)

type State = String
type Symbol = Char

type States = [State]
type Alphabet = [Symbol]

data Transition = Transition {
  source :: State,
  symbol :: Symbol,
  destination :: State
} deriving (Eq, Ord)

data FiniteAutomaton = FiniteAutomaton {
  states :: States,
  alphabet :: Alphabet,
  start_state :: State,
  accept_states :: States,
  transitions :: [Transition]
} deriving (Eq)

instance Show Transition where
  show (Transition source_t symbol_t destination_t) =
          source_t ++ "," ++ [symbol_t] ++ "," ++ destination_t
--          "(" ++ source_t ++ "," ++ [symbol_t] ++ "," ++ destination_t ++ ")"

instance Show FiniteAutomaton where
  show (FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transitions_a) =
          intercalate "," states_a ++ "\n" ++
          alphabet_a ++ "\n" ++
          start_state_a ++ "\n" ++
          intercalate ","  accept_states_a ++ "\n" ++
          intercalate "\n" (map show transitions_a)

data AllErrors = InvalidFileFormat | InvalidArguments | InvalidAutomatonFormat

instance Show AllErrors where
    show InvalidFileFormat = "Invalid input file format"
    show InvalidArguments = "Excepting argument -i or -t and optionally an input"
    show InvalidAutomatonFormat = "Automaton is not valid"