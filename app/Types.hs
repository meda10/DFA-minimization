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
} deriving (Eq)

data Finite_automaton = Finite_automaton {
  states :: States,
  alphabet :: Alphabet,
  start_state :: State,
  accept_states :: States,
  transition_function :: [Transition]
} deriving (Eq)

instance Show Transition where
  show (Transition source_t symbol_t destination_t) =
          source_t ++ "," ++ [symbol_t] ++ "," ++ destination_t

instance Show Finite_automaton where
  show (Finite_automaton states_a alphabet_a start_state_a accept_states_a transition_function_a) = 
          (intercalate "," $ map (id) states_a) ++ "\n" ++ 
          (alphabet_a) ++ "\n" ++
          (start_state_a) ++ "\n" ++
          (intercalate "," $ map (id) accept_states_a) ++ "\n" ++
          (intercalate "\n" $ map (show) transition_function_a)

data AllErrors
      = Invalid_file_format
      | Invalid_arguments

instance Show AllErrors where
    show Invalid_file_format = "Invalid input file format!"
    show Invalid_arguments = "Invalid Arguments!"