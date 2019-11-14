module Circle where

import           Graphics.Gloss
import           Parser
import           Test.QuickCheck
import Colors

data Circle
  = Pre Point Float [Int] Float
  | Cmp Point Float [[Point]] Float
  deriving (Show)


cirCen :: Circle -> Point
cirCen (Pre c _ _ _) = c
cirCen (Cmp c _ _ _) = c

cirRad :: Circle -> Float
cirRad (Pre _ r _ _) = r
cirRad (Cmp _ r _ _) = r

segmentCount :: Circle -> Int
segmentCount (Pre _ _ ss _) = length ss
segmentCount (Cmp _ _ ps _) = length ps

padding :: Circle -> Float
padding (Pre _ _ _ p) = p
padding (Cmp _ _ _ p) = p

dist :: Point -> Point -> Float
dist (x1, y1) (x2, y2) = sqrt $ (pow (x1 - x2)) + (pow (y1 - y2))
  where
    pow x = x * x

segments :: Circle -> [Int]
segments (Pre _ _ s _)  = s
segments (Cmp _ _ ps _) = map length ps

mkCircle :: Parser.Trans -> Point -> Float -> Float -> Circle
mkCircle t c r padding = Pre c r (transToSegs t) padding

transToSegs :: Parser.Trans -> [Int]
transToSegs t = [count seq s | s <- [0 .. symbs - 1]]
  where
    symbs = length $ symbols t
    seq = map (\(x, _) -> x) $ transSeq t

compile :: Circle -> Circle
compile (Cmp c r ps padding) = Cmp c r ps padding
compile cir@(Pre c r ss padding) = Cmp c r (allPoints cir) padding

allPoints :: Circle  -> [[Point]]
allPoints (Cmp _ _ ps padding) = ps
allPoints c@(Pre _ _ ss padding) =
  [ [subSegPoint c seg sub | sub <- [0 .. subs - 1]]
  | (seg, subs) <- zip [0 .. length ss - 1] ss
  ]

segLength :: Circle -> Int -> Int
segLength c i
  | length segs <= i = error "Can't access segments out of bounds."
  | otherwise = (segments c) !! i
  where
    segs = segments c

subSegPoint :: Circle -> Int -> Int -> Point
subSegPoint (Cmp _ _ ps padding) i j = (ps !! i) !! j
subSegPoint c@(Pre _ _ ss padding) i j =
  aToP c $
  (segSpan c) * (seg + padding) +
  ((effSegSpan c) * (sub + 0.5) / count)
  where
    seg = fromIntegral i
    sub = fromIntegral j
    count = fromIntegral $ ss !! i

effSegSpan :: Circle -> Float
effSegSpan c = segSpan c * (1 - padding c)

segSpan :: Circle -> Float
segSpan c = 2 * pi / fromIntegral (segmentCount c)

count :: Eq a => [a] -> a -> Int
count xs y = length [True | x <- xs, x == y]

aToP :: Circle -> Float -> Point
aToP (Pre (x, y) r _ _) a = (x + (r * cos a), y - r * sin a)
aToP (Cmp (x, y) r _ _) a = (x + (r * cos a), y - r * sin a)

transToLines :: Circle -> Parser.Trans -> [Color] -> ([Path], [(Color, Color)])
transToLines c@(Pre _ _ _ _) t cs = transToLines (compile c) t cs
transToLines c t cs = (zipList ps (drop 1 ps), zip cols (drop 1 cols))
  where
    seq = transSeq t
    cols = map (\(x, _) -> cs !! x) seq
    ps = [subSegPoint c i j | (i, j) <- seq]

allLines :: [Path] -> [(Color, Color)] -> Float -> [Picture]
allLines ps cols pers =
  [ gradientLine 10 p c1 c2 pers
  | (p, (c1, c2)) <- zip ps $ map (\(x, y) -> (tp x, tp y)) cols
  ]
  where
    tp c = withAlpha exp c
    exp :: Float
    exp = 1 / (log $ fromIntegral $ length ps)

gradientLine :: Float -> [Point] -> Color -> Color -> Float -> Picture
gradientLine pre ((x1, y1):(x2, y2):[]) c1 c2 p =
  Pictures [Color col line | (col, line) <- zip (getColors i) (splitLine i)]
  where
    i = round $ dist (x1, y1) (x2, y2) / pre
    dist :: Point -> Point -> Float
    dist (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)
    splitLine :: Int -> [Picture]
    splitLine i = [Line [p, p'] | (p, p') <- zip points (drop 1 points)]
      where
        points = getPoints i
    getColors :: Int -> [Color]
    getColors i = gradient i c1 c2 p
    getPoints :: Int -> [Point]
    getPoints i = [f j | j <- [0 .. i]]
      where
        f k = (x1 + (x2 - x1) * r, y1 + (y2 - y1) * r)
          where
            r = fromIntegral k / fromIntegral i

lineColor :: Int -> Color
lineColor i = darken i 0 white
  where
    darken :: Int -> Int -> Color -> Color
    darken i exp col
      | i < 0 = col
      | otherwise = darken (i - 2 ^ exp) (exp + 1) (dim col)

toDeg :: Float -> Float
toDeg a = a / (2 * pi) * 360

zipList :: [a] -> [a] -> [[a]]
zipList [] _          = []
zipList _ []          = []
zipList (x:xs) (y:ys) = [x, y] : zipList xs ys

prop_zipList :: Eq a => [a] -> [a] -> Bool
prop_zipList a b = checkEq (zipList a b) (zip a b)
  where
    checkEq :: Eq a => [[a]] -> [(a, a)] -> Bool
    checkEq a b =
      and
        [ head xys == x && head (tail xys) == y && length xys == 2
        | (xys, (x, y)) <- zip a b
        ]
