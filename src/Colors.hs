module Colors where

import           Graphics.Gloss

circleGradient :: [Color] -> Float -> Int -> [Color]
circleGradient (x:xs) p i = take i $ multiGradient ((x : xs) ++ [x]) p i

multiGradient :: [Color] -> Float -> Int -> [Color]
multiGradient [] 0 _ = []
multiGradient (x:y:xys) p i
  | i == length (x : y : xys) = take i (x : y : xys)
  | otherwise = (leftGrad j x y p) ++ multiGradient (y : xys) p (i - j)
  where
    j = div i $ length xys + 1

gradient :: Int -> Color -> Color -> Float -> [Color]
gradient i c1 c2 x =
  [ mixColors (x + (1 - x) * (1 - r)) (x + (1 - x) * r) c1 c2
  | j <- [0 .. i - 1]
  , let r = fromIntegral j / (fromIntegral i - 1)
  ]

normalize :: (Color, Color) -> Float -> (Color, Color)
normalize (c1, c2) x = (mixColors 1 x c1 c2, mixColors x 1 c1 c2)

leftGrad :: Int -> Color -> Color -> Float -> [Color]
leftGrad i c1 c2 x =
  [ mixColors (x + (1 - x) * (1 - r)) (x + (1 - x) * r) c1 c2
  | j <- [0 .. i - 1]
  , let r = fromIntegral j / fromIntegral i
  ]
