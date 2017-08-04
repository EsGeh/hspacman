module Vector2D(
	module Vector2D,
	module SGData.Vector2D,
) where

import SGData.Vector2D hiding ( (*|), (|*) )

type Pos a = Vec a
type Speed a = Vec a
type Size a = Vec a

(*|) :: Num t => t -> (t, t) -> (t, t)
scalar *| vec = (scalar * (vecX vec), scalar * (vecY vec))

(|*) :: (Num a) => Vec a -> a -> Vec a
(|*) = flip (*|)

normalize :: (Floating a, Ord a) => (a, a) -> (a, a)
normalize (x,y) =
	(x / lSafe, y / lSafe )
	where
		lSafe = if l >= 0.0001 then l else 1
		l = sqrt $ x**2 + y**2

rect :: Num a => Pos a -> Size a-> [Vec a]
rect pos (w,h) = [ pos, pos|+|(0,h), pos|+|(w,h), pos|+|(w,0), pos ]
