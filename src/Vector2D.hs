module Vector2D where

import SGData.Vector2D hiding ( (*|), (|*) )


scalar *| vec = (scalar * (vecX vec), scalar * (vecY vec))
(|*) :: (Num a) => Vec a -> a -> Vec a
(|*) = flip (*|)

normalize (x,y) = sqrt $ x**2 * y**2
