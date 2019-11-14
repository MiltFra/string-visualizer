module Renderer where

import           Circle
import           Graphics.Gloss
import           Parser
import Config
import Curve
import Colors

render :: Int -> ColCurve -> Picture
render i (DP (TL (tx, ty) curve) c1 c2) =
  Translate tx ty $ render i (DP curve c1 c2)
render i (DP (RT ang curve) c1 c2) = Rotate ang $ render i (DP curve c1 c2)
render i (DP curve c1 c2) = toPicture $ zip sps (gradient (length sps) c1 c2 Config.colorPersistence)
  where
    sps = splits i curve

drawAll :: Trans -> (Int -> [Color]) -> Picture
drawAll t cols =
  Config.finalTransformation t c $
  Pictures [drawLines lines lineCols, drawSegs c cols, drawLabels c (symbols t)]
  where
    c = mkCircle t (0, 0) Config.radius Config.segmentPadding
    (lines, lineCols) = transToLines c t (cols $ segmentCount c)

drawWithCurves :: Parser.Trans -> (Int -> [Color]) -> Picture
drawWithCurves t cols =
  Config.finalTransformation t c $
  Pictures [ Pictures $ map (render 10) parabs , Scale 1.05 1.05 $ Pictures [drawSegs c cols, drawLabels c (symbols t)] ]
  where
    c = mkCircle t (0, 0) Config.radius Config.segmentPadding
    parabs = transToParabolas c Config.pivotFunc t (cols $ segmentCount c)


drawLines :: [Path] -> [(Color, Color)] -> Picture
drawLines ps cols = Pictures $ allLines ps cols Config.colorPersistence

drawCirc :: Circle -> Picture
drawCirc (Pre (x, y) r _ _) = Color white $ translate x y $ Circle r

drawLabels :: Circle -> [Char] -> Picture
drawLabels c l
  | Config.doRenderLabels =
    Pictures [Rotate (angle i) (trans lab) | (i, lab) <- zip [0 ..] allLabels]
  | otherwise = Blank
  where
    allLabels =
      [Color white $ Scale 0.3 0.4 $ Translate (-50) 0 $ Text [x] | x <- l]
    angle i = 90 + (toDeg $ centerAngle c i)
    trans = Translate 0 ((cirRad c) * 1.1)

drawAllSubs :: Circle -> Picture
drawAllSubs c =
  Pictures
    [ Pictures [translate x y $ Color black $ circleSolid 2 | (x, y) <- sub]
    | sub <- allPoints c
    ]
  where
    segs = segments c

animateString :: String -> Float -> Float -> Picture
animateString str totalSecs secs
  | secs > totalSecs = animateString str totalSecs totalSecs
  | otherwise = drawAll (mkTrans $ take (min i $ length str) str) Config.colors
  where
    i = round $ (fromIntegral $ length str) * secs / totalSecs

animateAll :: Parser.Trans -> (Int -> [Color]) -> Float -> Float -> Picture
animateAll t cols totalSecs secs =
  Rotate ((-90) + (toDeg $ segSpan c * padding c / (-2))) $
  Pictures
    [ drawLines (take i lines) (take i lineCols)
    , Scale 1.1 1.1 (Pictures [drawSegs c cols, drawLabels c (symbols t)])
    ]
  where
    c = mkCircle t (0, 0) Config.radius Config.segmentPadding
    (lines, lineCols) = transToLines c t (cols $ segmentCount c)
    i = min j $ length lines
      where
        j = round $ (fromIntegral $ length lines) * secs / totalSecs

drawSegs :: Circle -> (Int -> [Color]) -> Picture
drawSegs c cols
  | doRenderSegments =
    Pictures
      [ Color col $ Rotate (step * fromIntegral i) a
      | (i, col) <- zip [1 .. segmentCount c] (cols $ segmentCount c)
      ]
  | otherwise = Blank
  where
    step = toDeg $ segSpan c
    start = (toDeg $ segSpan c * padding c / 2)
    a = ThickArc 0 (toDeg $ effSegSpan c) r 10
    r = cirRad c

centerAngle :: Circle -> Int -> Float
centerAngle c i =
  (segSpan c) * (padding c + fromIntegral i) +
  ((effSegSpan c) * 0.5)



