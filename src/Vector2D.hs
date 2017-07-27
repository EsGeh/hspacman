module Vector2D where

import SGData.Vector2D hiding ( (*|), (|*) )


(*|) :: Num t => t -> (t, t) -> (t, t)
scalar *| vec = (scalar * (vecX vec), scalar * (vecY vec))

(|*) :: (Num a) => Vec a -> a -> Vec a
(|*) = flip (*|)

normalize :: Floating a => (a, a) -> a
normalize (x,y) = sqrt $ x**2 * y**2
