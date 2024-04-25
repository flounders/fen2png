module Data.Game.Chess.FEN where

import Control.Applicative ((<|>))
import qualified Control.Monad
import Data.Char (isDigit)
import Data.List (uncons)
import qualified Data.Map as M
import Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S, sepBy1, skipSpaces)

data Piece
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving
    (Show)

data Player
  = White
  | Black
  deriving (Show)

data SquareContent = SquareContent {piece :: Piece, player :: Player} deriving (Show)

newtype Board = Board {squares :: M.Map Square SquareContent} deriving (Show)

data CastlingSide
  = QueenSide
  | KingSide
  deriving (Show)

data Game = Game
  { board :: Board
  , toMove :: Player
  , castling :: [(Player, CastlingSide)]
  , enPassant :: Maybe Square
  , halfMoves :: Int
  , fullMoves :: Int
  }
  deriving (Show)

newtype Square = Square {square :: String} deriving (Eq, Ord, Show)

parse :: String -> Maybe Game
parse s = do
  g <- uncons $ readP_to_S parseGame s
  return . fst $ fst g

parseGame :: ReadP Game
parseGame =
  Game
    <$> parseBoard
    <*> parseToMove
    <*> parseCastling
    <*> parseEnPassant
    <*> parseHalfMoves
    <*> parseFullMoves

parseBoard :: ReadP Board
parseBoard = do
  let p x = x `elem` (['1' .. '8'] ++ ['r', 'b', 'n', 'q', 'k', 'p', 'R', 'B', 'N', 'Q', 'K', 'P'])
      symbolToSquareContent x = case x of
        'r' -> SquareContent Rook Black
        'b' -> SquareContent Bishop Black
        'n' -> SquareContent Knight Black
        'q' -> SquareContent Queen Black
        'k' -> SquareContent King Black
        'p' -> SquareContent Pawn Black
        'R' -> SquareContent Rook White
        'B' -> SquareContent Bishop White
        'N' -> SquareContent Knight White
        'Q' -> SquareContent Queen White
        'K' -> SquareContent King White
        'P' -> SquareContent Pawn White
        _ -> error "symbolToSquareContent: Given character not matched"
      numberToFile x = case x of
        1 -> 'a'
        2 -> 'b'
        3 -> 'c'
        4 -> 'd'
        5 -> 'e'
        6 -> 'f'
        7 -> 'g'
        8 -> 'h'
        _ -> error "numberToFile: Given number not matched"
      f :: M.Map Square SquareContent -> (Int, String) -> (Int, M.Map Square SquareContent)
      f m (r, xs) =
        foldr
          ( \x (acc, m') ->
              if isDigit x
                then (read [x] + acc, m')
                else (acc + 1, maybe m' (\y -> M.insert y (symbolToSquareContent x) m') (toSquare [numberToFile acc, head $ show r]))
          )
          (1, m)
          xs
  ranks <- sepBy1 (munch1 p) (char '/')
  Control.Monad.when (length ranks /= 8) $ fail "Not all ranks were parsed"
  skipSpaces
  return . Board $ foldr (\x acc -> snd $ f acc x) M.empty (zip (reverse [1 .. 8]) ranks)

parseToMove :: ReadP Player
parseToMove = do
  p <- (char 'b' >> return Black) <|> (char 'w' >> return White)
  skipSpaces
  return p

parseCastling :: ReadP [(Player, CastlingSide)]
parseCastling = do
  let p x = x == 'K' || x == 'Q' || x == 'k' || x == 'q' || x == '-'
      f x acc = case x of
        'K' -> (Black, KingSide) : acc
        'Q' -> (Black, QueenSide) : acc
        'k' -> (White, KingSide) : acc
        'q' -> (White, QueenSide) : acc
        _ -> acc
  possibilities <- munch1 p
  skipSpaces
  return $ foldr f [] possibilities

parseEnPassant :: ReadP (Maybe Square)
parseEnPassant = do
  let p x = x `elem` ('-' : ['a' .. 'h'] ++ ['1' .. '8'])
  cs <- munch1 p
  skipSpaces
  return $ toSquare cs

parseInt :: ReadP Int
parseInt = do
  ds <- munch1 isDigit
  return $ read ds

parseHalfMoves :: ReadP Int
parseHalfMoves = do
  i <- parseInt
  skipSpaces
  return i

parseFullMoves :: ReadP Int
parseFullMoves = do
  i <- parseInt
  skipSpaces
  return i

toSquare :: String -> Maybe Square
toSquare [x, y] =
  let validFile = ['a' .. 'h']
      validRank = ['1' .. '8']
   in if elem x validFile && elem y validRank then Just $ Square [x, y] else Nothing
toSquare _ = Nothing
