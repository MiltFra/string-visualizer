module Curve where

import           Circle
import           Colors
import           Graphics.Gloss
import           Parser

type Line = (Point, Point)

data Curve
  = CV Int (Int -> Point)
  | TL Point Curve
  | RT Float Curve

data ColCurve =
  DP Curve Color Color


lineToPicture :: Line -> Color -> Picture
lineToPicture (p1, p2) col = Color col $ Line [p1, p2]

toPicture :: [(Line, Color)] -> Picture
toPicture xs = Pictures [lineToPicture p col | (p, col) <- xs]

splits :: Int -> Curve -> [Line]
splits i (CV len f) = zip segs (drop 1 segs)
  where
    segs = [f j | j <- [0 .. len - 1]]

listToCurve :: [Point] -> Curve
listToCurve ps = CV (length ps) ((!!) ps)

quadratic :: Int -> Float -> Float -> Curve
quadratic depth width a =
  listToCurve $ (getPoints depth (-width) width) ++ [pointAt width]
  where
    f = par a
    pointAt x = (x, f x)
    getPoints :: Int -> Float -> Float -> [Point]
    getPoints 0 l r = []
    getPoints d l r = pointAt l : getPoints (d - 1) l c ++ getPoints (d - 1) c r
      where
        c = (l + r) / 2

par :: Float -> Float -> Float
par a x = a * x * x

transToParabolas :: Circle -> (Float->Float)-> Parser.Trans -> [Color] -> [ColCurve]
transToParabolas c@(Pre _ _ _ _) f t cs = transToParabolas (compile c) f t cs
transToParabolas c f t cs =
  [ subSegsToParabola c f i1 j1 i2 j2 c1 c2
  | (((i1, j1), c1), ((i2, j2), c2)) <- zip ijcs $ drop 1 ijcs
  ]
  where
    seq = transSeq t
    alph = min (0.5) $ max 0.03 (1 - 0.9 * (fromIntegral $ length ijcs) / 1000)
    cols = map (\(x, _) -> withAlpha alph $ cs !! x) seq
    ijcs = zip seq cols

getLine :: Point -> Point -> Color -> Color -> ColCurve
getLine (p1x, p1y) (p2x, p2y) c1 c2 = DP (listToCurve $ list 20) c1 c2
  where
    center = ((p1x + p2x) / 2, (p1y + p2y) / 2)
    list i =
      [ (p1x * (1 - r) + p2x * r, p1y * (1 - r) + p2y * r)
      | j <- [0 .. i]
      , let r = fromIntegral j / f
      ]
      where
        f = fromIntegral i

subSegsToLine ::
     Circle -> Int -> Int -> Int -> Int -> Color -> Color -> ColCurve
subSegsToLine c i1 j1 i2 j2 = Curve.getLine p1 p2
  where
    p1 = subSegPoint c i1 j1
    p2 = subSegPoint c i2 j2

subSegsToParabola ::
     Circle -> (Float -> Float) -> Int -> Int -> Int -> Int -> Color -> Color -> ColCurve
subSegsToParabola c f i1 j1 i2 j2 = getParabolaLines p1 q p2
  where
    p1 = subSegPoint c i1 j1
    p2 = subSegPoint c i2 j2
    q = pivot c f p1 p2 $ proximity c i1 i2

getParabolaLines :: Point -> Point -> Point -> Color -> Color -> ColCurve
getParabolaLines p1 q p2 col1 col2 =
  DP (listToCurve $ take (length rounded - 1) rounded) col1 col2
  where
    rounded = roundify p1 q p2 0.5 4

convex :: Point -> Point -> Point -> Bool
convex (p1x, p1y) (p2x, p2y) (p3x, p3y) = vx * uy > vy * ux
  where
    vx = p1x - p2x
    vy = p1y - p2y
    ux = p3x - p2x
    uy = p3y - p2y

roundify :: Point -> Point -> Point -> Float -> Int -> [Point]
roundify p1 q p2 s i = (rList [p1, q, p2, (0, 0)] oldR s i) -- regarding the (0,0)... don't ask... basically it's necessary because the last element of the list is ignored
  where
    rem :: Eq a => [a] -> a -> [a]
    rem xs x = filter (/= x) xs
    con :: Float
    con
      | convex p1 q p2 = -1
      | otherwise = 1
    oldR = min 0.4 $ (dist (center p1 p2) q) / (dist p1 p2)
    rList :: [Point] -> Float -> Float -> Int -> [Point]
    rList ps _ _ 0 = ps
    rList ps r s i
      | r >= 0.4 = take (length ps) l ++ drop (length ps + 1) l
      | otherwise = l
      where
        l = rList newPS (r * s) s (i - 1)
        newPS :: [Point]
        newPS =
          foldr
            (++)
            []
            [[p1, corner p1 p2 (r * s) con] | (p1, p2) <- zip ps (drop 1 ps)]

center :: Point -> Point -> Point
center (p1x, p1y) (p2x, p2y) = (0.5 * (p1x + p2x), 0.5 * (p1y + p2y))

corner :: Point -> Point -> Float -> Float -> Point
corner p1@(p1x, p1y) p2@(p2x, p2y) r con = (mx + perpx * r, my + perpy * r)
  where
    (mx, my) = center p1 p2
    (perpx, perpy) = (con * (p2y - p1y), con * (p1x - p2x))

getParabola :: Point -> Point -> Point -> Color -> Color -> ColCurve
getParabola (p1x, p1y) (qx, qy) (p2x, p2y) col1 col2 =
  DP (TL (qx, qy) $ RT (360 - angle) $ quadratic 10 (2 * x) a) col1 col2
  where
    (mx, my) = ((p1x + p2x) / 2, (p1y + p2y) / 2)
    (ux, uy) = (nx / mg, ny / mg)
      where
        n@(nx, ny) = (p2x - p1x, p2y - p1y)
        mg = magnitude n
    (vx, vy) = (nx / mg, ny / mg)
      where
        n@(nx, ny) = (mx - qx, my - qy)
        mg = magnitude n
    a = y / (x ** 2)
    y = (uy * (p1x - qx) - ux * (p1y - qy)) / (vx * uy - vy * ux)
    x = (vy * (p1x - qx) - vx * (p1y - qy)) / (vy * ux - vx * uy)
    angle = toDeg $ 1 / (tan $ qy / qx)

magnitude :: Point -> Float
magnitude (vx, vy) = sqrt $ vx ** 2 + vy ** 2

proximity :: Circle -> Int -> Int -> Int
proximity c i j = min n p
  where
    p = mod (i - j) d
    n = mod (j - i) d
    d = segmentCount c

pivot :: Circle -> (Float->Float) ->Point -> Point -> Int -> Point
pivot c f (px, py) (qx, qy) p = ((1 - r) * mx + r * cx, (1 - r) * my + r * cy)
  where
    (mx, my) = ((px + qx) / 2, (py + qy) / 2)
    r = pivotRatio f c p
    (cx, cy) = cirCen c

pivotRatio :: (Float -> Float) -> Circle -> Int -> Float
pivotRatio f c p =
  f (fromIntegral p / fromIntegral (segmentCount c))
