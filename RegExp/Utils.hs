module RegExp.Utils where

import Data.Time.Clock

import RegExp.Parser
import RegExp.NFA
import RegExp.DFA

compileToNFA :: String -> Either String (NFA Int Char)
compileToNFA regexp =
    case parseRegExp regexp of
        Left err    ->  Left $ show err
        Right ast   ->  Right $ fromRegExp ast

compileToDFA :: String -> Either String (DFA Int Char)
compileToDFA regexp =
    case compileToNFA regexp of
        Left err    ->  Left $ show err
        Right nfa   ->  Right $ optimize $ fromNFA nfa
