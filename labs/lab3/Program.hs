-- Simon Lundberg

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)


newtype T = Program [Statement.T]

program :: Parser T
program = iter (spaces -# Statement.parse #- spaces) >-> Program

str :: T -> String
str (Program (s : stmts)) = toString s ++ "\n" ++ str (Program stmts)
str (Program []) = ""

instance Parse T where
  parse = program
  toString = str

exec :: Program.T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty
