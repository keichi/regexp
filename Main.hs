module Main where

import RegExp.Parser
import RegExp.NFA
import RegExp.DFA
import qualified Data.Set as Set

fromRight :: Either a b -> b
fromRight (Right a) = a
fromRight _ = error "Data.Either.Utils.fromRight: Left"

main = do
    let nfa = fromRegExp .fromRight . parseRegExp $ "test?"
        dfa = fromNFA nfa
   
    print dfa

    print $ runNFA nfa "tes"
    print $ runDFA dfa "testa"
