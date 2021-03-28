module Minimize where

import Data.List  
import Types
  

-------------------------------------------------------------

sinkState :: String
sinkState = "x" 
  
createSinkTransition :: [(State, Symbol)] -> [Transition]
createSinkTransition transitions = [Transition source_t symbol_t destination_t | tr <- transitions, source_t <- [fst tr], symbol_t <- [snd tr], destination_t <- [sinkState]]   
  
allTransitions :: [a] -> [b] -> [(a, b)]
allTransitions _ [] = [] 
allTransitions states_a alphabet_a = [(source_t, symbol_t) | source_t <- states_a, symbol_t <- alphabet_a]

existingTransitions :: [Transition] -> [(State, Symbol)]
existingTransitions [] = []
existingTransitions transitions = [(source_t, symbol_t) | tr <- transitions, source_t <- [source tr] , symbol_t <- [symbol tr]]

missingTransitions :: Eq a => [a] -> [a] -> [a]
missingTransitions [] _ = []     
missingTransitions all_tr [] = all_tr     
missingTransitions all_tr existing_tr = all_tr \\ existing_tr


createSinkState :: FiniteAutomaton -> FiniteAutomaton
createSinkState fin_a@(FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transition_function_a)
        = if null missing_tr then fin_a else fin_a { states = states_a ++ [sinkState],
                                                      alphabet = alphabet_a,
                                                      start_state = start_state_a,
                                                      accept_states = accept_states_a,
                                                      transition_function = transition_function_a ++ missing_tr}
          where
            all_tr = allTransitions (states fin_a) (alphabet fin_a)
            existing_tr = existingTransitions (transition_function fin_a)
            missing = missingTransitions all_tr existing_tr
            missing_tr = createSinkTransition missing

-------------------------------------------------------------
              