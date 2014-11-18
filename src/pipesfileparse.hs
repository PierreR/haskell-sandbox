-- | Main entry point to the application.
module Main where

import           Pipes
import           Pipes.Attoparsec (parsed)
import qualified Pipes.Text.IO as PT
import           Data.Attoparsec.Text (Parser, decimal, endOfLine, space)
import qualified System.IO as IO

import           Control.Applicative
import           Control.Monad (void)

data Test = Test {
  _a :: Int,
  _b :: Int
  } deriving (Show)

testParser :: Parser Test
testParser =
  pure Test
  <*> decimal <* space
  <*> decimal

main :: IO ()
main = void $ IO.withFile "testfile" IO.ReadMode $ \h -> runEffect $
    for (parsed (testParser <* optional endOfLine) (PT.fromHandle h)) (lift . print)
