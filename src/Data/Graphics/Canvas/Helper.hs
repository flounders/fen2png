module Data.Graphics.Canvas.Helper where

import Data.Graphics.Canvas (Canvas (..), Pixel, putPixel)

drawCircle ::
  Canvas ->
  Pixel ->
  Double ->
  Int ->
  Int ->
  IO ()
drawCircle c p radius xOffset yOffset =
  let maxCanvasLength = fromInteger . toInteger $ max canvasHeight canvasWidth
      canvasHeight = height c
      canvasWidth = width c
      sampleSize = maxCanvasLength * 4
      radians = [0.0, 2 * pi / sampleSize .. 2 * pi]
      pixels :: [(Int, Int)]
      pixels = filter (\(x, y) -> x < canvasWidth && y < canvasHeight && x > 0 && y > 0) $ map f radians
      f x = ((xOffset +) . round . (radius *) $ cos x, (yOffset +) . round . (radius *) $ sin x)
   in mapM_ (\(x, y) -> putPixel x y p c) pixels
