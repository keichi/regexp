module RegExp.DFA where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.List hiding ((\\))

import RegExp.NFA

type DFAState = Set NFAState
type DFATransition a = (DFAState, a, DFAState)

data DFA a = DFA {
    dfaStates   ::  Set DFAState,
    dfaTrans    ::  Set (DFATransition a),
    dfaStart    ::  DFAState,
    dfaFinish   ::  Set DFAState
} deriving (Eq, Show)

runDFA :: (Ord a) => DFA a -> [a] -> Bool
runDFA (DFA states trans start finish) str =
    foldl' step start str `Set.member` finish
    where
        step current c =
            let next = Set.filter (\(s, arc, _) -> s == current && c == arc) trans
            in if Set.null next
                then current
                else (\(_, _, f) -> f) $ Set.findMin next

fromNFA :: (Ord a) => NFA a -> DFA a
fromNFA nfa = DFA {
        dfaStates   =   states,
        dfaTrans    =   trans,
        dfaStart    =   start,
        dfaFinish   =   Set.filter (\s -> nfaFinish nfa `Set.member` s) states
    }
    where
        start = closure nfa $ Set.singleton $ nfaStart nfa
        (states, trans) = buildDFA nfa (Set.singleton start) Set.empty $ Set.singleton start

buildDFA :: (Ord a) => NFA a -> Set DFAState -> Set (DFATransition a) -> Set DFAState -> (Set DFAState, Set (DFATransition a))
buildDFA nfa states trans todo
    | Set.null todo = (states, trans)
    | otherwise = buildDFA nfa (states `Set.union` newStates) (trans `Set.union` newTransitions) (newStates \\ states)
    where
        newStates = Set.map (\(_, _, f) -> closure nfa f) newTransitions
        newTransitions = Set.unions $ map (buildDFATrans nfa . closure nfa) $ Set.toList todo

buildDFATrans :: (Ord a) => NFA a -> DFAState -> Set (DFATransition a)
buildDFATrans nfa@(NFA _ nts _ _) state =
    Set.fromList $ map combine $ groupByArc dts
    where
        combine transitions =
            let (_, c:_, fs) = unzip3 transitions
            in (state, c, closure nfa $ Set.fromList fs)
        groupByArc arcs =
            let characters = nub $ map (\(_, c, _) -> c) arcs
            in map (\c -> filter (\(_, c', _) -> c'== c) arcs) characters
        dts = [(s, c, f) | (s, Just c, f) <- Set.toList nts, s `Set.member` state]
