{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Write Expr.T |
    While Expr.T Statement |
    Begin [Statement] |
    Read String |
    Skip |
    Repeat Statement Expr.T

    deriving Show

statement :: Parser Statement
statement = assignment ! conditional ! write ! while ! begin ! Statement.read ! skip ! Statement.repeat
statements = iter statement

-- Assignment parsing
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
    where buildAss (v, e) = Assignment v e

-- If parsing
conditional = accept "if" -# Expr.parse #- require "then" # statement #- require "else" # statement >-> buildIf
    where buildIf ((condition, ifstate), elsestate) = If condition ifstate elsestate

-- write parsing
write = accept "write" -# Expr.parse #- require ";" >-> Write

-- while parsing
while = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
    where buildWhile (ex, st) = While ex st

-- begin parsing
begin = accept "begin" -# statements #- require "end" >-> Begin

-- read parsing
read = accept "read" -# word #- require ";" >-> Read

-- skip parsing
skip = accept "skip;" >-> buildSkip
    where buildSkip _ = Skip

-- repeat parsing
repeat = accept "repeat" -# statement #- require "until" # Expr.parse #- require ";" >-> buildRepeat
    where buildRepeat (st, ex) = Repeat st ex

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment v e : stmts) dict input = exec stmts newDictionary input -- assignment
    where newDictionary = Dictionary.insert (v, Expr.value e dict) dict
exec (If cond thenStmts elseStmts: stmts) dict input =  -- If
    if Expr.value cond dict>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Write v : stmts) dict input = Expr.value v dict : exec stmts dict input -- Write
exec (While ex st : stmts) dict input = -- While
    if Expr.value ex dict > 0
    then exec (st : (While ex st : stmts)) dict input
    else exec stmts dict input
exec (Begin s : stmts) dict input = exec (s ++ stmts) dict input -- Begin
exec (Read r: stmts) dict (i:input) = exec stmts (Dictionary.insert (r, i) dict) input -- Read
exec (Skip : stmts) dict input = exec stmts dict input -- Skip
exec (Repeat st ex : stmts) dict input = exec (st : If ex Skip (Repeat st ex) : stmts) dict input -- Repeat
exec _ _ _ = []


instance Parse Statement where
  parse = statement
  toString = error "Statement.toString not implemented"
