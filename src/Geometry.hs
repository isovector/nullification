module Geometry where

import Types
import Linear.Matrix

lineBetween :: V2 -> V2 -> Line
lineBetween a b = Line a $ b - a

lineRel :: V2 -> V2 -> Line
lineRel a rel = Line a rel

-- from https://hackage.haskell.org/package/SG-1.0/docs/src/Data-SG-Geometry-TwoDim.html#intersectLines2
lineIntersect' :: Line -> Line -> Maybe (Double, Double)
lineIntersect' (Line (V2 x y)   (V2 xd yd))
               (Line (V2 x' y') (V2 xd' yd'))
  | a == 0 = Nothing
  | otherwise = Just $ (t, t')
  where
    a = (xd' * yd) - (xd * yd')
    t' = ((xd * (y' - y)) - (yd * (x' - x))) / a
    t = ((xd' * (y - y')) - (yd' * (x - x'))) / (negate a)

linesIntersection :: Line -> Line -> Maybe V2
linesIntersection (Line astart aend) (Line bstart bend) =
    onLine $
      lineIntersect'
        (Line astart rel)
        (Line bstart $ bend - bstart)
  where
    rel = aend - astart
    onLine Nothing       = Nothing
    onLine (Just (x, y)) = if x >= 0 && x <= 1 && y >= 0 && y <= 1
                              then Just $ x *^ rel + astart
                              else Nothing

linesIntersect :: Line -> Line -> Bool
linesIntersect a b = onLine $ lineIntersect' a b
  where onLine Nothing       = False
        onLine (Just (x, y)) = x >= 0 && x <= 1 && y >= 0 && y <= 1


inRect :: Box -> V2 -> Bool
inRect (Rectangle (V2 x y) (V2 w h))
       (V2 px py) =  x  <= px
                  && px <  x + w
                  && y  <= py
                  && py <  y + h


rectLines :: Box -> [Line]
rectLines (Rectangle xy@(V2 x y) wh@(V2 w h)) =
  [ Line xy  $ V2 (x + w) y
  , Line xy  $ V2 x (y + h)
  , Line (V2 (x + w) y) xy'
  , Line (V2 x (y + h)) xy'
  ]
  where
    xy' = xy + wh

lineIntersectsRect :: Line -> Box -> Bool
lineIntersectsRect line = any (isJust . linesIntersection line) . rectLines


moveBox :: V2 -> Box -> Box
moveBox dxy (Rectangle xy wh) = Rectangle (dxy + xy) wh


rotateV2 :: Angle -> V2 -> V2
rotateV2 (Radians dir) v2 = rotation !* v2
  where
    rotation = V2 (V2 (cos dir) (-sin dir))
                  (V2 (sin dir) (cos dir))


------------------------------------------------------------------------------
-- | Clamp a vector.
clamp :: V2 -> V2 -> V2 -> V2
clamp (V2 lx ly) (V2 ux uy) (V2 x y) = V2 (clamp' lx ux x) (clamp' ly uy y)

clamp' :: Ord a => a -> a -> a -> a
clamp' l u z = min u $ max l z
