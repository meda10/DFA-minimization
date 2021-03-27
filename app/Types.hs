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



--intercalate ", " ["Lorem", "ipsum", "dolor"]
--
--instance Show CFG where
--  show CFG{..} = unlines $
--    [intercalate "," $ map (: []) nonterminals] ++
--    [intercalate "," $ map (: []) terminals] ++
--    [[startingSymbol]] ++
--    map (\(l, r) -> [l] ++ "->" ++ r) rules
--
--instance Show Automata where
--    -- "AUTOMATA whith alphabet " ++ show e ++ ":\n" ++ header for alphabet output
--    show (A q e d q0 f) = displayListInline displayState q ++ "
--    \n"
--        ++ displayState q0 ++ "\n"
--        ++ displayListInline displayState f  ++
--        if not $ null d
--            then "\n" ++ displayListNewline show d
--            else ""
--
--displayListInline func = intercalate "," . map func