module Types where

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
    "("++show source_t++","++show symbol_t++","++show destination_t++")\n"

instance Show Finite_automaton where
  show (Finite_automaton states_a alphabet_a start_state_a accept_states_a transition_function_a) =
        "S = " ++ show states_a ++ "\n" ++
        "Σ = " ++ show alphabet_a ++ "\n" ++
        "S0 = " ++ show start_state_a ++ "\n" ++
        "F = " ++ show accept_states_a ++ "\n" ++
        "δ = " ++ show transition_function_a ++ "\n"

data AllErrors 
      = Invalid_file_format
      | Invalid_arguments

instance Show AllErrors where
    show Invalid_file_format = "Invalid input file format!"
    show Invalid_arguments = "Invalid Arguments!"