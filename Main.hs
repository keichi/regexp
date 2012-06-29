module Main where

import Text.Show.Pretty

import RegExp.NFA
import RegExp.DFA
import RegExp.Utils

fromRight :: Either a b -> b
fromRight (Right a) = a
fromRight _ = error "Data.Either.Utils.fromRight: Left"

main = do
    let nfa = fromRight $ compileToNFA "a(a|b)*"
        dfa = fromRight $ compileToDFA "a(a|b)*"

    putStrLn $ nfaToDot nfa
    putStrLn $ dfaToDot dfa

    --putStrLn $ ppShow nfa
    --putStrLn $ ppShow dfa

    --print $ runNFA nfa "hadooop"
    --print $ runDFA dfa "Hadp"
    

