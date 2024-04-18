module Data.Game.Chess.Render where

import Data.Graphics.Canvas (
  Canvas (height, width),
  Pixel,
  putPixel,
 )

drawBoard :: Canvas -> Pixel -> Pixel -> IO ()
drawBoard c light dark
  | height c /= width c = fail "drawBoard: Canvas should be square."
  | otherwise = do
      let sideLength = height c
          boardSquareSideLength = div sideLength 8
          drawRank x y
            | even $ div y boardSquareSideLength =
                putPixel x y (if even $ div x boardSquareSideLength then light else dark) c
            | otherwise = putPixel x y (if even $ div x boardSquareSideLength then dark else light) c
      sequence_ [drawRank x y | x <- [0 .. sideLength - 1], y <- [0 .. sideLength - 1]]
