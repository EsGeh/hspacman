{-# LANGUAGE TemplateHaskell #-}
module GameData where

import Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)
--import Control.Monad.Identity
import System.Random( StdGen )
import Lens
import Lens.Micro.Platform


-- |Directions in Labyrinth
data Direction = Up | Down | Right | Left deriving (Eq, Show)
type Movement = Direction

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
	world_dbgInfo :: DebugInfo,
	world_randomGen :: StdGen
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

data GhostState = GhostState {
	ghost_dir_history :: [(Direction, Float)]
} deriving(Show)

type Dot = Object ()
type Fruit = Object ()
type Pacman = Object ()
type Ghost = Object GhostState

-- default objects
defObj :: Pos Float -> Object ()
defObj pos = Object{
	obj_pos = pos |+| (0.1, 0.1) ,
	obj_size = (0.8,0.8),
	obj_direction = (0,0),
	obj_t = 0,
	obj_state = ()
}
defGhost :: Pos Float -> Ghost
defGhost pos =
	(defObj pos){ obj_state = GhostState{ ghost_dir_history = [ (Right, 0)] } }

data UIState = Playing | Menu | GameOver | Won deriving(Show)

makeLensesWith lensRules' ''World
makeLensesWith lensRules' ''Object
makeLensesWith lensRules' ''GhostState

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
