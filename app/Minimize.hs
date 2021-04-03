--  Project: 
--  Author: Petr Medek 
--  Year: 2021

module Minimize where

import Data.List
import Types

-------------------------------------------------------------

sinkState :: String
sinkState = "x"

sinkStateTransitions :: [Symbol] -> [Transition]
sinkStateTransitions alphabet_a = [Transition sinkState symbol_t sinkState | symbol_t <- alphabet_a]
  
createSinkTransition :: [(State, Symbol)] -> [Transition]
createSinkTransition transitions_t = [Transition source_t symbol_t sinkState | tr <- transitions_t, source_t <- [fst tr], symbol_t <- [snd tr]]

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
                                                      transitions = transitions_a ++ missing_tr ++ sink_state_transitions}
          where
            all_tr = allTransitions (states fin_a) (alphabet fin_a)
            existing_tr = existingTransitions (transitions fin_a)
            missing = missingTransitions all_tr existing_tr
            missing_tr = createSinkTransition missing
            sink_state_transitions = sinkStateTransitions alphabet_a

-------------------------------------------------------------

accessibleStates :: [State] -> [State] -> [Transition] -> [State]
accessibleStates [] _ _ = []
accessibleStates _ _ [] = []
accessibleStates states_s old_states transitions_t =
        if states_s == old_states
        then states_s
        else do
          let new_states = nub $ sort (states_s ++ [ q | tr <- transitions_t, st <- states_s, q <- [destination tr], source tr == st]) --todo check maybe `elem`
          accessibleStates new_states states_s transitions_t


removeUnusedStates :: FiniteAutomaton -> FiniteAutomaton
removeUnusedStates fin_a@(FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transitions_a) =
        fin_a { states = new_states,
                alphabet = alphabet_a,
                start_state = start_state_a,
                accept_states = intersect new_states accept_states_a,
                transitions = new_transitions}
          where
            new_states = accessibleStates [start_state_a] [] transitions_a
            new_transitions = [ tr | tr <- transitions_a, source tr `elem` new_states && destination tr `elem` new_states]

-------------------------------------------------------------

nonEmptyIntersect :: Eq a => [a] -> [a] -> Bool
nonEmptyIntersect [] _ = False
nonEmptyIntersect _ [] = False
nonEmptyIntersect a b = intersect a b /= []

nonEmptyDifference :: Eq a => [a] -> [a] -> Bool
nonEmptyDifference [] _ = False
nonEmptyDifference _ [] = False
nonEmptyDifference a b = (a \\ b) /= []

transitionLeadsTo :: Foldable t => Symbol -> t State -> [Transition] -> [State]
transitionLeadsTo _ _ [] = []
transitionLeadsTo sym dst transitions_t = [ source_t | tr <- transitions_t, source_t <- [source tr], destination_t <- [destination tr] , symbol_t <- [symbol tr], destination_t `elem` dst && symbol_t == sym]

---------------------------------------

doSomething :: Ord a => [[a]] -> [[a]] -> [a] -> [a] -> ([[a]], [[a]])
doSomething p w x y =
  if y `elem` w then
    (new_p, new_w)
  else
    if length intersection <= length difference then
      (new_p, w_add_intersection)
    else
      (new_p, w_add_difference)
  where
    intersection = intersect x y
    difference = y \\ x
    new_p = insert difference $ insert intersection $ delete y p
    new_w = insert difference $ insert intersection $ delete y w
    w_add_intersection = insert intersection w
    w_add_difference = insert difference w


forEachXY :: Ord a => [[a]] -> [[a]] -> [a] -> [[a]] -> ([[a]], [[a]])
forEachXY p w _ [] = (p, w)
forEachXY p w x (y: y_tail) = forEachXY mod_p mod_w x y_tail
  where
    (mod_p, mod_w) = if nonEmptyDifference y x && nonEmptyIntersect x y then doSomething p w x y else (p, w)

foreachLetterAlphabet :: Foldable t => [[State]] -> [[State]] -> t State -> [Symbol] -> [Transition] -> ([[State]], [[State]])
foreachLetterAlphabet p w _ [] _ = (p, w)
foreachLetterAlphabet p w a (alphabet_head : alphabet_tail) transitions_a = foreachLetterAlphabet mod_p mod_w a alphabet_tail transitions_a
  where
    x = transitionLeadsTo alphabet_head a transitions_a
    (mod_p, mod_w) = forEachXY p w x p


mainWhile :: [[State]] -> [[State]] -> [Symbol] -> [Transition] -> States
mainWhile p [] _ _ = delete "" [ intercalate "_" w | w <- p]
mainWhile p (a : w) alphabet_a transitions_a = mainWhile mod_p mod_w alphabet_a transitions_a
  where
    (mod_p, mod_w) = foreachLetterAlphabet p w a alphabet_a transitions_a

-------------------------------------------------------------

isSubset1 :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
isSubset1 a b = all (`elem` b) a

getStateNames :: [[Char]] -> [[Char]] -> [[Char]]
getStateNames old new = [ createStartState new state | state <- old]

createFinalStates :: [[Char]] -> [[Char]] -> [[Char]]
createFinalStates accept_s states_s = nub [ createStartState states_s state | state <- accept_s]

createStartState :: [[Char]] -> [Char] -> [Char]
createStartState states_s start_state_s = if start_state_s `elem` states_s then start_state_s
  else concat new_start_s
  where
    new_start_s = [ state | state <- states_s, ("_"++start_state_s) `isSubset1` state || (start_state_s++"_") `isSubset1` state]


-------------------------------------------------------------

renameStates :: Eq a1 => [a1] -> [a2] -> [a2]
renameStates old new = [ new !! index | state <- old, state `elem` old, Just index <- [elemIndex state old]]

renameFinalStates :: Eq a1 => [a1] -> [a2] -> [a1] -> [a2]
renameFinalStates states_s reamed_s accept_s = [ reamed_s !! index | state <- accept_s, Just index <- [elemIndex state states_s]]

renameTransitions :: [State] -> [State] -> [Transition] -> [Transition]
renameTransitions old new transitions_t = nub [ Transition (new !! index_src) symbol_t (new !! index_dst) | tr <- transitions_t, source_t <- [source tr],
        symbol_t <- [symbol tr], destination_t <- [destination tr], source_t `elem` old, Just index_src <- [elemIndex source_t old],
        destination_t `elem` old, Just index_dst <- [elemIndex destination_t old]]

getStatesPositionsInTransition :: [Transition] -> [State]
getStatesPositionsInTransition transitions_t = nub (concat [[src,dst] | tr <- transitions_t, src <- [source tr], dst <- [destination tr]])

renameStartStateInStates :: [State] -> State -> [State]
renameStartStateInStates states_s start_state_s  = [ if start_state_s == state then "0" else if state == "0" then "y" else state | state <- states_s]

getIndex :: Eq a => [a] -> [a] -> [String]
getIndex old positions = [ show index | state <- old, state `elem` positions, Just index <- [elemIndex state positions]]

renameAll :: [State] -> [State] -> State -> [State] -> [Transition] -> ([State], State, [State], [Transition])
renameAll old_states new_states start_state_a accept_states_a transitions_a =
        (states_s, start_state_s, fin_states, transitions_t)
          where
            states_s = renameStates old_states new_states
            start_state_s = concat (renameFinalStates old_states new_states [start_state_a])
            fin_states = renameFinalStates old_states new_states accept_states_a
            transitions_t = sort (renameTransitions old_states new_states (sort transitions_a))

renameAutomaton :: [State] -> [Char] -> [State] -> [Transition] -> ([State], [Char], [State], [Transition])
renameAutomaton states_s start_state_s accept_states_s transitions_t =
        (s, st, accept, tr)
          where
            renamed_start_state = renameStartStateInStates states_s start_state_s
            (s1, st1, accept1, tr1) = renameAll states_s renamed_start_state start_state_s accept_states_s transitions_t
            renamed_states = getIndex s1 (getStatesPositionsInTransition tr1)
            (s, st, accept, tr) = renameAll s1 renamed_states st1 accept1 tr1


minimalAutomaton :: FiniteAutomaton -> FiniteAutomaton
minimalAutomaton fin_a@(FiniteAutomaton states_a alphabet_a start_state_a accept_states_a transitions_a) =
        fin_a { states = sort s,
                alphabet = sort alphabet_a,
                start_state = sort st,
                accept_states = sort accept,
                transitions = sort tr}
          where
            p = [accept_states_a, states_a \\ accept_states_a]
            new_states = mainWhile p p alphabet_a (sort transitions_a)
            new_start_state = createStartState new_states start_state_a
            new_transitions = sort $ nub (renameTransitions states_a (getStateNames states_a new_states) transitions_a)
            new_accept_states = createFinalStates accept_states_a new_states
            (s, st, accept, tr) =  renameAutomaton new_states new_start_state new_accept_states new_transitions

--sortBy (\(a,_) (b,_) -> compare a b) [(2, "world"), (4, "!"), (1, "Hello")]
--[(1,"Hello"),(2,"world"),(4,"!")]
