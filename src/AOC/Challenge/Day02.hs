{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day02 (
      day02a
    , day02b
  ) where

import           AOC.Prelude
import           Text.Megaparsec as TM ( Parsec, anySingle, parseMaybe )
import           Text.Megaparsec.Char       (char, space)
import           Text.Megaparsec.Char.Lexer (decimal)

day02a :: [Policy] :~> Int
day02a = MkSol
    { sParse = traverse (parseMaybe policy) . lines
    , sShow  = show
    , sSolve = validate validatePart1
    }

day02b :: [Policy] :~> Int
day02b = MkSol
    { sParse = traverse (parseMaybe policy) . lines
    , sShow  = show
    , sSolve = validate validatePart2
    }


data Policy = P Int Int Char String deriving (Show, Eq, Ord, Generic)

type Parser = Parsec Policy String

policy :: Parser Policy
policy = P <$> decimal
           <*> (char '-' *> decimal)
           <*> (space *> anySingle)
           <*> (char ':' *> space *> AOC.Prelude.some anySingle)

validate :: (Foldable f, Functor f) => (a -> Bool) -> f a -> Maybe Int
validate f xs = Just $ countTrue (== True) (fmap f xs)

validatePart1 :: Policy -> Bool
validatePart1 (P pMin pMax pChar password) = n >= pMin && n <= pMax
  where
    n = countTrue (== pChar) password

validatePart2 :: Policy -> Bool
validatePart2 (P pIndex1 pIndex2 pChar password) = n == 1
  where
    n = countTrue (== pChar) [password !! (pIndex1 - 1), password !! (pIndex2 - 1)]
