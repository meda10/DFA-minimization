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
} deriving (Show, Eq)




--instance Show Finite_automaton where
--   show (Finite_automaton)


instance Show Transition where
  show (Transition source destination symbol) = 
    "("++show source++" x "++show symbol++" -> "++show destination++")"

--instance Show Finite_automaton where
--  show (Finite_automaton states alphabet transition_function start_state accept_states) =     
--        "states:\n" ++ show states ++ "\n" ++
--        "symbols:\n" ++ show alphabet ++ "\n" ++
--        "trans:\n" ++ show transition_function ++ "\n" ++
--        "init:\n" ++ show start_state ++ "\n" ++
--        "final:\n" ++ show accept_states ++ "\n"





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