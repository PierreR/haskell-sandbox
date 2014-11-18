module Parsing where
import Text.ParserCombinators.Parsec
-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec                as Parsec

-- I am the error message infix operator, used later:
import           Text.Parsec                ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import           Control.Applicative

-- Get the Identity monad from here:
import           Control.Monad.Identity     (Identity)

-- alias Parsec.parse for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text

myParser :: Parsec.Parsec String () (String,String)
myParser = do
    letters <- Parsec.many1 Parsec.letter
    Parsec.spaces
    digits <- Parsec.many1 Parsec.digit
    return (letters,digits)

integer :: Parser Int
integer = read <$> many1 digit
