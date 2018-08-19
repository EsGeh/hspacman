{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module GameData(
	module GameData.Types,
	module GameData,
) where

import GameData.Types
import Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)
import Data.Maybe
import Data.Tuple( swap )
import Lens.Micro.Platform

------------------------------------------
-- game state:
------------------------------------------


------------------------------------------
-- helper functions
------------------------------------------

-- default objects
defObj :: Pos Float -> Object ()
defObj pos = Object{
	obj_pos = pos |+| (0.1, 0.1) ,
	obj_size = (0.8,0.8),
	obj_direction = (0,0),
	-- obj_t = 0,
	obj_state = ()
}
defGhost :: Pos Float -> Ghost
defGhost pos =
	set (obj_state_l) (GhostState []) $
	defObj pos

objCenter :: Object objState -> Pos Float
objCenter Object{..} =
	obj_pos |+| 0.5 *| obj_size

-- |return the path destination, if any
path_dest :: Path a -> Maybe (Pos a)
path_dest = listToMaybe . reverse

{-
class ToText a where
	toText :: a -> String
-}

instance (Show a) => Show (Matrix a) where
	show = showMatr

showMatr :: Show a => Matrix a -> String
showMatr =
	unlines .
	map (foldl (\x y -> x ++ "|" ++ y) "" . map show) .
	mGetAllRows

-- |a view on the area of surrounding terrain
nextFields :: Size Int -> Pos Int -> Labyrinth -> [(Pos Int, Territory)]
nextFields size pos labyrinth =
	map (\p -> (p, mGet (swap p) labyrinth)) $
	surroundingPositions (mGetSize labyrinth) size pos

-- |positions around a given viewpoint
surroundingPositions :: Size Int -> Size Int -> Pos Int -> [Pos Int]
surroundingPositions fieldSize size pos =
	do
		deltaX <- [-vecX size..vecX size]
		deltaY <- [-vecY size..vecY size]
		return $
			pointInSize fieldSize $
			pos |+| (deltaX, deltaY)

opposite :: Direction -> Direction
opposite Left = Right
opposite Right = Left
opposite Up = Down
opposite Down = Up

leftOf :: Direction -> Direction
leftOf Left = Down
leftOf Up = Left
leftOf Right = Up
leftOf Down = Right

rightOf :: Direction -> Direction
rightOf Left = Up
rightOf Up = Right
rightOf Right = Down
rightOf Down = Left

orthogonal :: Direction -> [Direction]
orthogonal d = [ ret | ret<-allDirs, ret/=d, ret/=opposite d ]

allDirs :: [Direction]
allDirs = [Up,Down,Left,Right]

directionsToSpeed :: Num a => [Direction] -> Vec a
directionsToSpeed = foldl (|+|) (0,0) . map directionToSpeed

directionToSpeed :: Num a => Direction -> Vec a
directionToSpeed Up = (0,-1)
directionToSpeed Down = (0,1)
directionToSpeed Left = (-1,0)
directionToSpeed Right = (1,0)

speedToDirection :: (Eq a, Num a) => Vec a -> [Direction]
speedToDirection speed = 
	let 
		xDir = case (signum $ vecX speed) of
			1 -> [Right]
			(-1) -> [Left]
			0 -> []
			_ -> error "internal error"
		yDir = case (signum $ vecY speed) of
			1 -> [Down]
			(-1) -> [Up]
			0 -> []
			_ -> error "internal error"
	in
		xDir ++ yDir

