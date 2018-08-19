{-# LANGUAGE TemplateHaskell #-}
module GameData.Types where

import Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)
import Data.Tuple( swap )
import Lens
import Lens.Micro.Platform


------------------------------------------
-- game state:
------------------------------------------

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
newtype Labyrinth = Labyrinth{ fromLabyrinth :: Matrix Terrain }
	-- deriving( Foldable )

data Terrain = Free | Wall
	deriving(Read, Show, Eq, Ord)
instance Enum Terrain where
	toEnum int = case int of
		0 -> Free
		1 -> Wall
		_ -> error "Terrain enum error"
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
makeLensesWith lensRules' ''Labyrinth

instance Show Labyrinth where
	show = showMatr . fromLabyrinth

showMatr :: Show a => Matrix a -> String
showMatr =
	unlines .
	map (foldl (\x y -> x ++ "|" ++ y) "" . map show) .
	mGetAllRows

-- Labyrinth functions:

labyrinth_fromRows :: [[Terrain]] -> Maybe Labyrinth
labyrinth_fromRows =
	fmap Labyrinth . mFromListRow

-- |ATTENTION: coordinates in (x, y)
labyrinth_get :: Pos Int -> Labyrinth -> Terrain
labyrinth_get pos =
	mGet (swap pos) . fromLabyrinth

-- |ATTENTION: coordinates in (x, y)
labyrinth_set :: Pos Int -> Terrain -> Labyrinth -> Labyrinth
labyrinth_set pos terrain =
	over fromLabyrinth_l $ mSet (swap pos) terrain

-- |ATTENTION: (x-Size, y-Size) = (#cols, #rows)
labyrinth_size :: Labyrinth -> Size Int
labyrinth_size =
	swap . mGetSize . fromLabyrinth

labyrinth_width :: Labyrinth -> Int
labyrinth_width =
	vecX . labyrinth_size

labyrinth_height :: Labyrinth -> Int
labyrinth_height =
	vecY . labyrinth_size

labyrinth_allPositions :: Labyrinth -> [Pos Int]
labyrinth_allPositions lab =
	do
		x <- [0..(labyrinth_width lab)-1]
		y <- [0..(labyrinth_height lab)-1]
		return (x,y)

labyrinth_mapWithIndex :: (Pos Int -> Terrain -> b) -> Labyrinth -> Matrix b
labyrinth_mapWithIndex f lab =
	mapWithIndex `flip` (fromLabyrinth lab) $ \pos x ->
		f (swap pos) x
