module Vector2D(
	module Vector2D,
	module SGData.Vector2D,
) where

import SGData.Vector2D hiding ( (*|), (|*) )
import Data.Fixed

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
		l = vec_length (x,y)

vec_length (x,y) = sqrt $ x**2 + y**2

rect :: Num a => Pos a -> Size a-> [Vec a]
rect pos (w,h) =
	[ pos
	, pos|+|(0,h)
	, pos|+|(w,h)
	, pos|+|(w,0)
	, pos
	]


pointInSize :: Integral a => Size a -> Pos a -> Size a
pointInSize (width,height) (x,y)  = (x `mod` width, y `mod` height)

pointInSizeF :: Size Float -> Pos Float -> Size Float
--pointInSizeF :: (Real a) => Size a -> Pos a -> Size a
pointInSizeF (width,height) (x,y)  =
	( (x) `safeRealMod` width
	, (y) `safeRealMod` height
	)

safeRealMod :: Float -> Float -> Float
--safeRealMod :: Real a => a -> a -> a
safeRealMod x m =
	let res = x `mod'` m
	in
		if res >= m - 0.00001
		then 0
		else res

movePoint :: Integral a => Size a -> Pos a -> Speed a -> Pos a
movePoint size_ pos_ dir = pointInSize size_ (pos_ |+| dir)
