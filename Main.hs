module Main where

import RegExp.Parser
import RegExp.NFA
import RegExp.DFA
import qualified Data.Set as Set

fromRight :: Either a b -> b
fromRight (Right a) = a
fromRight _ = error "Data.Either.Utils.fromRight: Left"

main = do
    let nfa = fromRegExp .fromRight . parseRegExp $ "(a|b)*ab"
        nfa2 = fromRegExp .fromRight . parseRegExp $ "a(a|b)*"
        nfa3 = fromRegExp .fromRight . parseRegExp $ "x*x*x*x*x*x*x*x*ax*x*x*x*x*x*x*x*x*"
        dfa = fromNFA nfa
   
    putStrLn $ toDot nfa3

    print dfa
    print $ runDFA dfa "aaabaab"
    print $ runDFA dfa "aba"
    print $ runDFA dfa "ab"

    print $ runNFA nfa3 "xxxxxxxxxxxxxxxxxxaxxxxxxxxxxxxx"
    print $ runNFA nfa3 "a"
    print $ runNFA nfa3 "b"
