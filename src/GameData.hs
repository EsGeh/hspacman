{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module GameData where

import Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)
import Data.Maybe
import Data.Tuple( swap )
import Lens
import Lens.Micro.Platform


-- |Directions in Labyrinth
data Direction = Up | Down | Right | Left deriving (Eq, Show)
type Movement = Direction

type Path a = [Pos a]

--type Area a = (Pos a, Size a)

type Time = Float
type DeltaT = Float


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

data GameState
	= Playing World
	| Menu
	| GameOver Statistics
	| Won Statistics
	-- deriving(Show)

data World = World {
	world_userInput :: CurrentKeys,
	world_t :: Time,
	world_statistics :: Statistics,
	world_labyrinth :: Labyrinth,
	world_pacman :: Pacman,
	world_ghosts :: [Ghost],
	world_dots :: [Dot],
	world_fruits :: [Fruit],
	world_pacmanSpeed :: Float,
	world_ghostSpeed :: Float,
	world_dbgInfo :: DebugInfo
	-- world_randomGen :: StdGen
}
	-- deriving(Show)

data Statistics = Statistics {
	world_level :: Level,
	world_points :: Points
}
	deriving(Show)

type CurrentKeys = [Direction]

type DebugInfo = [String]
{-
data DebugInfo = DbgInf {
	info :: String
} deriving(Show)
-}

type Level = Int
type Points = Int

-- |ATTENTION:
-- |rows represent up-down, columns left-right on screen.
-- |This means if you have screen corrds (x,y)
-- |you might wanna look up `mGet (y,x) labyrinth`
type Labyrinth = Matrix Territory

data Territory = Free | Wall
	deriving(Read, Show, Eq, Ord)
instance Enum Territory where
	toEnum int = case int of
		0 -> Free
		1 -> Wall
		_ -> error "Territory enum error"
	fromEnum ter = case ter of
		Free -> 0
		Wall -> 1

data Object objState = Object {
	obj_pos :: Pos Float,
	obj_size :: Size Float,
	obj_direction :: Speed Float,
	--obj_t :: Time,
	obj_state :: objState
} deriving( Show, Eq, Ord )

data GhostState = GhostState {
	-- a path the ghost is currently trying to follow
	ghost_pathToDest :: Path Int
} deriving(Read, Show, Eq, Ord)

type Dot = Object ()
type Fruit = Object ()
type Pacman = Object ()
type Ghost = Object GhostState

makeLensesWith lensRules' ''World
makeLensesWith lensRules' ''Statistics
makeLensesWith lensRules' ''Object
makeLensesWith lensRules' ''GhostState

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

path_dest = listToMaybe . reverse

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
	map (\pos -> (pos, mGet (swap pos) labyrinth)) $
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
