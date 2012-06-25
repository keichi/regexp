module RegExp.DFA where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.List hiding ((\\))

import RegExp.NFA

type DFATransition s a = (s, a, s)

data DFA s a = DFA {
    dfaStates   ::  Set s,
    dfaTrans    ::  Set (DFATransition s a),
    dfaStart    ::  s,
    dfaFinish   ::  Set s
} deriving (Eq, Show)

optimize :: (Ord s, Ord a) => DFA s a -> DFA Int a
optimize (DFA states trans start finish) =
    DFA {
        dfaStates   =   Set.map toInt states,
        dfaTrans    =   Set.map (\(s, c, f) -> (toInt s, c, toInt f)) trans,
        dfaStart    =   toInt start,
        dfaFinish   =   Set.map toInt finish
    }
    where
        toInt set = case (lookup set lut) of
            Just id ->  id
            Nothing ->  error "RegExp.DFA.minimize - LUT error"
        lut = zip (Set.toList states) [0..]

runDFA :: (Ord s, Ord a) => DFA s a -> [a] -> Bool
runDFA (DFA states trans start finish) str =
    case (foldl' (\s c -> s >>= step c) (Just start) str) of
        Just state  ->  state `Set.member` finish
        Nothing     ->  False
    where
        step c current =
            let next = Set.filter (\(s, arc, _) -> s == current && c == arc) trans
            in if Set.null next
                then Nothing
                else (\(_, _, f) -> return f) $ Set.findMin next

fromNFA :: (Ord s, Ord a) => NFA s a -> DFA (Set s) a
fromNFA nfa = DFA {
        dfaStates   =   states,
        dfaTrans    =   trans,
        dfaStart    =   start,
        dfaFinish   =   Set.filter (\s -> nfaFinish nfa `Set.member` s) states
    }
    where
        start = closure nfa $ Set.singleton $ nfaStart nfa
        (states, trans) = buildDFA nfa (Set.singleton start) Set.empty $ Set.singleton start

buildDFA :: (Ord s, Ord a) => NFA s a -> Set (Set s) -> Set (DFATransition (Set s) a) -> Set (Set s) -> (Set (Set s), Set (DFATransition (Set s) a))
buildDFA nfa states trans todo
    | Set.null todo = (states, trans)
    | otherwise = buildDFA nfa (states `Set.union` newStates) (trans `Set.union` newTransitions) (newStates \\ states)
    where
        newStates = Set.map (\(_, _, f) -> closure nfa f) newTransitions
        newTransitions = Set.unions $ map (buildDFATrans nfa . closure nfa) $ Set.toList todo

buildDFATrans :: (Ord s, Ord a) => NFA s a -> Set s -> Set (DFATransition (Set s) a)
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
