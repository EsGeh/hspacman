module Move.Collision where

import GameData
import Vector2D

import SGData.Matrix
import Data.Tuple( swap )


willCollideWithLabyrinth :: Labyrinth -> Speed Float -> Float -> Object st -> Bool
willCollideWithLabyrinth lab direction dt obj =
	any (willPointCollideWithLabyrinth lab direction dt) $
	rect (obj_pos obj) (obj_size obj)

willPointCollideWithLabyrinth :: Labyrinth -> Speed Float -> Float -> Pos Float -> Bool
willPointCollideWithLabyrinth lab dir dt pos =
	pointCollidesWithLabyrinth lab $ 
	pos |+| (dir |* dt)

pointCollidesWithLabyrinth :: Labyrinth -> Pos Float -> Bool
pointCollidesWithLabyrinth lab =
	(==Wall) .
	(mGet `flip` lab) .
	swap .
	vecMap floor .
	pointInSizeF (fromIntegral $ mGetWidth lab, fromIntegral $ mGetHeight lab) -- torus

rectCollidesRect :: (Pos Float, Size Float) -> (Pos Float, Size Float) -> Bool
rectCollidesRect r1 r2 =
	rectCollidesRect' r1 r2 || rectCollidesRect' r2 r1

rectCollidesRect' :: (Pos Float, Size Float) -> (Pos Float, Size Float) -> Bool
rectCollidesRect' (pos1, size1) (pos2, size2) =
	any (pointCollidesRect (pos2, size2)) $
	rect pos1 size1

pointCollidesRect :: (Pos Float, Pos Float) -> Pos Float -> Bool
pointCollidesRect (rPos, rSize) p =
	and $
	[ fst p >= fst rPos
	, snd p >= snd rPos
	, fst p <= fst (rPos |+| rSize)
	, snd p <= snd (rPos |+| rSize)
	]
