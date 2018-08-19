{-# LANGUAGE TemplateHaskell #-}
module GameData.Types where

import Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)
import Lens
import Lens.Micro.Platform


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
}
	-- deriving(Show)

data Statistics = Statistics {
	world_level :: Level,
	world_points :: Points
}
	deriving(Show)

type CurrentKeys = [Direction]

type DebugInfo = [String]

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

------------------------------------------
-- simple types
------------------------------------------

-- |Directions in Labyrinth
data Direction = Up | Down | Right | Left deriving (Eq, Show)
type Movement = Direction

type Path a = [Pos a]

type Time = Float
type DeltaT = Float

makeLensesWith lensRules' ''World
makeLensesWith lensRules' ''Statistics
makeLensesWith lensRules' ''Object
makeLensesWith lensRules' ''GhostState
