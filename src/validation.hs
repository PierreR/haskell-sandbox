{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

-- import Control.Applicative.Lift
import           System.Exit
import           Text.Printf
import           Text.Read
import Data.List (intercalate)
import Control.Lens

validateFile :: FilePath -> IO()
validateFile fp =
  either (die . intercalate "\n") print .
  fmap sum . getValidation . sequenceA . -- sequenceA append errMsg using the applicative definition of Validation
  map (Validation . over _Left (:[])) .  -- _Left allows us to apply a function on the Left part from String to [String]. This will enable use to intercalate "\n" later on.
  zipWith parseNum [1..] .
  lines =<< readFile fp

  -- This bit was used to map using Errors from transformers instead of a custom Validation type
  -- where
  --   mapToErrors :: [Either String Int] -> [Errors String Int]
  --   mapToErrors = map (either failure Pure)


-- This type exist in the either package
-- Note that there is no way to make a monad out of it
data Validation e a = Validation {getValidation :: Either e a}
                  deriving Functor

instance Monoid e => Applicative (Validation e) where
  pure = Validation . Right
  Validation a <*> Validation b = Validation $
    case a of
      Right va -> fmap va b
      Left ea -> either (Left . mappend ea) (const $ Left ea) b

-- There is another equivalent but more readable way to define the applicative
-- instance Monoid e => Applicative (Validation e) where
--   pure  = Validation . Right
--   Validation (Left e) <*> Validation (Left e') = Validation (Left (e <> e'))
--   Validation (Left e) <*> Validation (Right _) = Validation $ Left e
--   Validation (Right _) <*> Validation (Left e') = Validation$ Left e'
--   Validation (Right f) <*> Validation (Right x) = Validation $ Right (f x)

parseNum
  :: Int -- line number (for error reporting)
  -> String -- line contents
  -> Either String Int
     -- either parsed number or error message
parseNum ln str =
  case readMaybe str of
    Just num -> Right num
    Nothing  -> Left $ printf "Bad number on line %d: %s" ln str

-- printSum2 :: FilePath -> IO ()
-- printSum2 path =
--   either die print .
--   liftM sum .
--   zipWithM parseNum [1..] .
--   lines =<< readFile path

main = validateFile "/home/vagrant/numbers.txt"
