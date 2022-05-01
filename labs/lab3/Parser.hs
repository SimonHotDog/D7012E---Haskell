-- Simon Lundberg

module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b -- Applies two parsers in sequence, discards second result
m -# n = m # n >-> snd

(#-) :: Parser a -> Parser b -> Parser a -- Applies two parsers in sequence, discards second result
m #- n = m # n >-> fst

spaces :: Parser String -- Finds all spaces in a string
spaces = iter (char ? isSpace)

token :: Parser a -> Parser a -- Removes all spaces in a string
token m = m #- spaces

letter :: Parser Char -- single letter
letter = char ? isAlpha

word :: Parser String -- single word
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String -- n chars
chars 0 = return []
chars n = char # chars(n-1) >-> cons

accept :: String -> Parser String -- Was already implemented
accept w = token (chars (length w)) ? (==w)

require :: String -> Parser String
require w  = accept w ! err w

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

