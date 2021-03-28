module Minimize where

import Data.List  
import Types
  

-------------------------------------------------------------

sinkState :: String
sinkState = "x"
  
createSinkTransition :: [(State, Symbol)] -> [Transition]
createSinkTransition transitions_t = [Transition source_t symbol_t destination_t | tr <- transitions_t, source_t <- [fst tr], symbol_t <- [snd tr], destination_t <- [sinkState]]

allTransitions :: [a] -> [b] -> [(a, b)]
allTransitions _ [] = []
allTransitions states_a alphabet_a = [(source_t, symbol_t) | source_t <- states_a, symbol_t <- alphabet_a]

existingTransitions :: [Transition] -> [(State, Symbol)]
existingTransitions [] = []
existingTransitions transitions_t = [(source_t, symbol_t) | tr <- transitions_t, source_t <- [source tr] , symbol_t <- [symbol tr]]

missingTransitions :: Eq a => [a] -> [a] -> [a]
missingTransitions [] _ = []
missingTransitions all_tr [] = all_tr
missingTransitions all_tr existing_tr = all_tr \\ existing_tr

createSinkState :: FiniteAutomaton -> FiniteAutomaton
createSinkState fin_a@(FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transitions_a) =
        if null missing_tr then fin_a else fin_a { states = states_a ++ [sinkState],
                                                      alphabet = alphabet_a,
                                                      start_state = start_state_a,
                                                      accept_states = accept_states_a,
                                                      transitions = transitions_a ++ missing_tr}
          where
            all_tr = allTransitions (states fin_a) (alphabet fin_a)
            existing_tr = existingTransitions (transitions fin_a)
            missing = missingTransitions all_tr existing_tr
            missing_tr = createSinkTransition missing

-------------------------------------------------------------

removeUnusedTransitions :: Foldable t => t State -> [Transition] -> [Transition]
removeUnusedTransitions states_s transitions_t = [ tr | tr <- transitions_t, source tr `elem` states_s && destination tr `elem` states_s]

accessibleStatesIteration :: [State] -> [Transition] -> [State]
accessibleStatesIteration [] _ = []
accessibleStatesIteration _ [] = []
accessibleStatesIteration states_s transitions_t = states_s ++ [ q | tr <- transitions_t, st <- states_s, q <- [destination tr], source tr == st]

accessibleStates :: [State] -> [State] -> [Transition] -> [State]
accessibleStates [] _ _ = []
accessibleStates _ _ [] = []
accessibleStates states_s old_states transitions_t =
        if states_s == old_states
        then states_s
        else do
          let st = nub $ sort (accessibleStatesIteration states_s transitions_t)
          accessibleStates st states_s transitions_t


removeUnusedStates :: FiniteAutomaton -> FiniteAutomaton
removeUnusedStates fin_a@(FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transitions_a) =
        fin_a { states = new_states,
                alphabet = alphabet_a,
                start_state = start_state_a,
                accept_states = intersect new_states accept_states_a,
                transitions = new_transitions}
          where
            new_states = accessibleStates [start_state_a] [] transitions_a
            new_transitions = removeUnusedTransitions new_states transitions_a
