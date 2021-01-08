{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable


module AOC.Challenge.Day08 (
    day08a
  -- , day08b
  ) where

import           AOC.Prelude
import           Text.Megaparsec as TM      ( Parsec, anySingle, parseMaybe )
import           Text.Megaparsec.Char       (char, space, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Text.Megaparsec hiding (State)
import           Control.Monad.State  


data Operation = Acc | Jmp | Nop deriving (Eq, Ord, Show)
data Instruction = Instruction Operation Int deriving (Eq, Ord, Show)
type Program = [Instruction]
type Accumulator = Int
type PC = Int
type Tracing = [Int]
type ProgramState = (PC, Accumulator, Tracing)


day08a :: Program :~> Int
day08a = MkSol
    { sParse = traverse (parseMaybe pInstruction) . lines
    , sShow  = show
    , sSolve = \program -> Just $ evalState (run program) (0, 0, [])
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }


type Parser = Parsec Instruction String

pInstruction :: Parser Instruction
pInstruction = do
  opcode  <- pOperation
  _ <- space
  sign <- choice [char '+' >> return 1, char '-' >> return (-1), return 1]
  operand <- decimal 
  return $ Instruction opcode (sign * operand)

pOperation :: Parser Operation
pOperation = choice
  [ Acc <$ string "acc"
  , Jmp <$ string "jmp"
  , Nop <$ string "nop" ]


run :: Program -> State ProgramState Accumulator
run program = do
      (pc, _, tracing) <- get
      acc' <- eval (program!!pc)
      if pc `elem` tracing then
        return acc'
      else
        run program

eval :: Instruction -> State ProgramState Accumulator
eval (Instruction Acc operand) = state $ \(pc, acc, tracing) -> (acc, (pc + 1,       acc + operand, pc:tracing))
eval (Instruction Jmp operand) = state $ \(pc, acc, tracing) -> (acc, (pc + operand, acc,           pc:tracing))
eval (Instruction Nop _)       = state $ \(pc, acc, tracing) -> (acc, (pc + 1,       acc,           pc:tracing))
  