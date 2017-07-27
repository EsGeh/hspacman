module GameData where

import SGData.Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)
import Data.Fixed

import System.Random


-- |Directions in Labyrinth
data Direction = Up | Down | Right | Left deriving (Eq, Show)
type Movement = Direction

type Pos a = Vec a
type Speed a = Vec a
type Size a = Vec a
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

data World = World {
	world_uiState :: UIState,
	world_userInput :: CurrentKeys,
	world_level :: Level,
	world_points :: Points,
	world_labyrinth :: Labyrinth,
	world_pacman :: Pacman,
	world_ghosts :: [Ghost],
	world_dots :: [Dot],
	world_fruits :: [Fruit],
	world_dbgInfo :: DebugInfo
}
	-- deriving(Show)

type CurrentKeys = [Direction]

data DebugInfo = DbgInf {
	info :: String
} deriving(Show)

type Level = Int
type Points = Int

type Labyrinth = Matrix Territory
data Territory = Free | Wall deriving(Show,Eq)
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
	obj_t :: Time,
	obj_state :: objState
} deriving(Show)

{-
data GhostState = GhostState {
	rndState :: StdGen
} deriving(Show)
-}

type Dot = Object ()
type Fruit = Object ()
type Pacman = Object ()
type Ghost = Object ()

defObj pos = Object{
	obj_pos = pos,
	obj_size = (1,1),
	obj_direction = (0,0),
	obj_t = 0,
	obj_state = ()
}

data UIState = Playing | Menu deriving(Show)

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

pointInSize :: Integral a => Size a -> Pos a -> Size a
pointInSize (width,height) (x,y)  = (x `mod` width, y `mod` height)
pointInSizeF :: (Real a) => Size a -> Pos a -> Size a
pointInSizeF (width,height) (x,y)  = (x `mod'` width, y `mod'` height)

movePoint :: Integral a => Size a -> Pos a -> Speed a -> Pos a
movePoint size_ pos_ dir = pointInSize size_ (pos_ |+| dir)

-- realizes a "torus like" behavior for positions on the field
{-getNeighbourIndex :: Size -> MatrIndex -> Movement -> MatrIndex
getNeighbourIndex (width,height) pos@(x,y) dir = case dir of
	Up -> (x,(y-1) `niceMod` height)
	Right -> ((x+1) `niceMod` width, y)
	Down -> (x,(y+1) `niceMod` height)
	Left -> ((x-1) `niceMod` width, y)
	where
		niceMod val m = case signum val of
			(-1) -> niceMod (val+m) m
			(1) -> val `mod` m
			(0) -> 0
			otherwise -> error "niceMod internal error!"
-}

{-data ObjectType = Dot | Fruit
data MovableObjType = PacMan | Monster-}
