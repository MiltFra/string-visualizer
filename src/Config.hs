module Config where

import           Colors
import           Graphics.Gloss
import Circle
import Parser

size :: (Int, Int)
size = (800, 800)

center :: (Int, Int)
center = (div (fst size) 2, div (snd size) 2)

radius :: Float
radius = 300

colors :: Int -> [Color]
colors = circleGradient gradientInputs 0.0

gradientInputs :: [Color]
gradientInputs = map (dark) primary

primary = [red, green, blue]
secondary = [yellow, cyan, magenta]
tertiary = [aquamarine, orange, violet]
other = [dark blue, dark red, white]

colorPersistence :: Float
colorPersistence = 0.3

doRemoveConsecutiveDuplicates :: Bool
doRemoveConsecutiveDuplicates = True

doLimitInputLength :: Bool
doLimitInputLength = True

inputLengthLimit :: Int
inputLengthLimit = 50000

doRenderLabels :: Bool
doRenderLabels = True

doRenderSegments :: Bool
doRenderSegments = True

doCurveTransitions :: Bool
doCurveTransitions = True

segmentPadding :: Float
segmentPadding = 0.1

finalTransformation :: Trans -> Circle -> Picture -> Picture
finalTransformation t c = Rotate ((-90) + (toDeg $ segSpan c * segmentPadding / (-2)))

pivotFunc :: Float -> Float
pivotFunc r = c + k * (1-c) * r
  where
    c = 0.3
    k = 2