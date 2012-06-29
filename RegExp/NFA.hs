module RegExp.NFA (
        NFA(..),
        NFAFTransition,
        toDot,
        fromRegExp,
        runNFA,
        closure
) where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.List hiding ((\\))
import Control.Monad.State

import RegExp.Parser

-- |Type synonym for a NFA transition function
type NFAFTransition s a = (s, Maybe a, s)

-- |Data type representing a Dterminisitc Finite Automata
data NFA s a = NFA {
    nfaStates   ::  Set s,
    nfaTrans    ::  Set (NFAFTransition s a),
    nfaStart    ::  s,
    nfaFinish   ::  s
} deriving (Eq, Show)

-- |Converts the given NFA to a graphviz dot format
toDot :: NFA Int Char -> String
toDot nfa =
    unlines $ ["digraph nfa {"] ++ body ++ ["}"]
    where
        body = Set.toList $ Set.map conv $ nfaTrans nfa
        conv (n1, Just c, n2) = show n1 ++ " -> " ++ show n2
            ++ " [label=" ++ [c] ++ "]"
        conv (n1, Nothing, n2) = show n1 ++ " -> " ++ show n2

-- |Build a NFA from the given regular expression syntax tree
fromRegExp :: (Ord a) => RegExp a -> NFA Int a
fromRegExp re = evalState (fromRegExp' re) 0

fromRegExp' :: (Ord a) => RegExp a -> State Int (NFA Int a)
fromRegExp' Epsilon = do
    s <- get
    modify (+1)
    f <- get
    modify (+1)
    return NFA {
        nfaStart    =   s,
        nfaFinish   =   f,
        nfaStates   =   Set.fromList [s, f],
        nfaTrans    =   Set.singleton (s, Nothing, f)
    }
fromRegExp' (Literal c) = do
    s <- get
    modify (+1)
    f <- get
    modify (+1)
    return NFA {
        nfaStart    =   s,
        nfaFinish   =   f,
        nfaStates   =   Set.fromList [s, f],
        nfaTrans    =   Set.singleton (s, Just c, f)
    }
fromRegExp' (Then r1 r2) = do
    (NFA states1 trans1 s1 f1) <- fromRegExp' r1
    (NFA states2 trans2 s2 f2) <- fromRegExp' r2
    
    return NFA {
        nfaStart    =   s1,
        nfaFinish   =   f2,
        nfaStates   =   states1 `Set.union` states2,
        nfaTrans    =  Set.unions [
            trans1,
            trans2,
            Set.singleton (f1, Nothing, s2)
            ]
    }
fromRegExp' (Or r1 r2) = do
    (NFA states1 trans1 s1 f1) <- fromRegExp' r1
    (NFA states2 trans2 s2 f2) <- fromRegExp' r2
    s <- get
    modify (+1)
    f <- get
    modify (+1)

    return NFA {
        nfaStart    =   s,
        nfaFinish   =   f,
        nfaStates   =  Set.unions [
            states1,
            states2,
            Set.singleton s,
            Set.singleton f
            ],
        nfaTrans    =  Set.unions [
            trans1,
            trans2,
            Set.fromList [
                (s, Nothing, s1),
                (s, Nothing, s2),
                (f1, Nothing, f),
                (f2, Nothing, f)
                ]
            ]
    }
fromRegExp' (Star r1) = do
    (NFA states1 trans1 s1 f1) <- fromRegExp' r1
    s <- get
    modify (+1)
    f <- get
    modify (+1)

    return NFA {
        nfaStart    =   s,
        nfaFinish   =   f,
        nfaStates   =  Set.unions [
            states1,
            Set.singleton s,
            Set.singleton f
            ],
        nfaTrans    =  Set.unions [
            trans1,
            Set.fromList [
                (s, Nothing, s1),
                (f1, Nothing, s1),
                (s1, Nothing, f)
                ]
            ]
    }
fromRegExp' (Optional r1) =
    fromRegExp' $ Or Epsilon r1
fromRegExp' (Plus r1) =
    fromRegExp' $ Then r1 $ Star r1

-- |Run the NFA with the given input, and determines if the input is accepted
runNFA :: (Ord s, Eq a) => NFA s a -> [a] -> Bool
runNFA nfa str =
     nfaFinish nfa `Set.member` foldl' step init str
     where
        step states c = closure nfa $ onemove nfa c states
        init = closure nfa $ Set.singleton $ nfaStart nfa

-- |Computes the states that are transitionable from the vurrent state with
-- |the given input
onemove :: (Ord s, Eq a) => NFA s a -> a -> Set s -> Set s
onemove (NFA _ trans _ _) c =
    Set.unions . Set.toList . Set.map onemove'
    where
        onemove' state = Set.fromList [f | (s, Just arc, f) <- Set.toList trans,
            c == arc, s == state]

-- |Computes the epsilon-closure for the given states
closure :: (Ord s) => NFA s a -> Set s -> Set s
closure (NFA _ trans _ _) states =
    loop states states
    where
        loop done todo
            | Set.null todo = done
            | otherwise = loop (done `Set.union` closure' todo) (closure' todo \\ done)
        closure' = Set.unions . Set.toList . Set.map epsilon
        epsilon state = Set.fromList [f | (s, Nothing, f) <- Set.toList trans,
            s == state]
