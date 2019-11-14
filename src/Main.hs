module Main where

import           Config
import           Data.Char
import           Graphics.Gloss
import           Parser
import           Renderer
import           System.Environment
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  str <- readFile (head args)
  display window background (drawing $ atMost 100000 $ removeConsDuplicates str)
  where
    window = InWindow "String Visualization" size Config.center
    background = black

radius :: Float
radius = 4 / 5 * dim
  where
    dim = (fromIntegral (min (fst size) (snd size))) / 2

removeConsDuplicates :: Eq a => [a] -> [a]
removeConsDuplicates [] = []
removeConsDuplicates [x] = [x]
removeConsDuplicates (x:y:xys)
  | x == y = removeConsDuplicates (y : xys)
  | otherwise = x : removeConsDuplicates (y : xys)

atMost :: Int -> [a] -> [a]
atMost i xs
  | length xs > i = take i xs
  | otherwise = xs

drawing :: String -> Picture
drawing str
  | Config.doCurveTransitions = drawWithCurves (mkTrans $ filter isAlphaNum str) Config.colors
  | otherwise = drawAll (mkTrans $ filter isAlphaNum str) Config.colors

filterInput :: String -> String
filterInput str = applyFuncList funcs str
  where
    funcs =
      [ if doRemoveConsecutiveDuplicates
          then removeConsDuplicates
          else id
      , if doLimitInputLength
          then (atMost inputLengthLimit)
          else id
      ]

applyFuncList :: [(a -> a)] -> a -> a
applyFuncList fs = foldr (.) id fs
