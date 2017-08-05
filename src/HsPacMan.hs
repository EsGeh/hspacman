{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GameData
import LevelGenerator
import Renderpipeline
-- import SGData.Vector2D hiding ( (*|), (|*) )
import Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)

import Graphics.Gloss hiding(display)
import Graphics.Gloss.Interface.Pure.Game hiding(Up,Down)
import qualified Graphics.Gloss.Interface.Pure.Game as G

import Data.Tuple
import Data.List
import Control.Monad.Random

import Lens.Micro.Platform


windowTitle :: String
windowTitle = "hsPacMan"
windowPos :: Pos Float
windowPos = (100, 100) 
windowSize :: Pos Float
windowSize = (800, 600)

main :: IO ()
main = play
	display
	bgColour
	framerate
	(genWorld 0)
	(renderWorld windowSize) -- calls renderWorld from Module Renderpipeline
	handleInput
	moveWorld

display :: Display
display = InWindow windowTitle (vecMap floor windowSize) (vecMap floor windowPos)

bgColour :: Color
bgColour = black

framerate :: Int
framerate = 40

handleInput :: Event -> World -> World
handleInput event world = case event of
	(EventKey key upOrDown _ _) -> case (world_uiState world) of
		Menu -> case upOrDown of
			G.Down -> case key of
				Char 's' -> setUIState (genWorld 8) Playing
				_ -> world --alternative menue
			_ -> world --alternative menue
		Playing -> case upOrDown of
			G.Down -> case key of
				Char 'w' -> world{ world_userInput= addDir Up }
				Char 's' -> world{ world_userInput= addDir Down }
				Char 'a' -> world{ world_userInput= addDir Left }
				Char 'd' -> world{ world_userInput= addDir Right }
				SpecialKey KeySpace -> setPacDir world (0,0)
				_ -> world
			G.Up -> case key of
				Char 'w' -> world{ world_userInput= remDir Up }
				Char 's' -> world{ world_userInput= remDir Down }
				Char 'a' -> world{ world_userInput= remDir Left }
				Char 'd' -> world{ world_userInput= remDir Right }
				SpecialKey KeySpace -> setPacDir world (0,0)
				_ -> world
		where
			addDir dir = [dir] `union` (remDir $ opposite dir)
			remDir dir = filter (/=dir) currentKeys
			currentKeys = world_userInput world
	_ -> world


setUIState :: World -> UIState -> World
setUIState world state = world {world_uiState = state}

-- |changes the moving direction of the pacman
setPacDir :: World -> Speed Float -> World
setPacDir world dir = world { world_pacman = (world_pacman world) {obj_direction=dir}}

moveWorld :: DeltaT -> World -> World
moveWorld deltaT =
	movePacman deltaT
	.
	moveGhosts deltaT

moveGhosts :: DeltaT -> World -> World
moveGhosts dt world =
	let (newWorld, newRnd) =
		runRand `flip` (world_randomGen world) $
		traverseOf world_ghosts_l (mapM $ moveGhost world dt) world
	in
		newWorld{ world_randomGen = newRnd }

moveGhost :: forall m . MonadRandom m => World -> DeltaT -> Ghost -> m Ghost
moveGhost world dt ghost =
	do
		direction <- calcDirection :: m Direction
		let newLastDecision =
			if direction /= lastDir then obj_t ghost else lastDecision :: Float
		return $
			set (obj_state_l . ghost_dir_history_l) [(direction, newLastDecision)] $
			moveObj (world_labyrinth world) dt ((normalizeDir . directionToSpeed) $ direction) speed ghost
	where
		speed = 2
		[(lastDir, lastDecision)] = ghost_dir_history $ obj_state $ ghost
		calcDirection :: m Direction
		calcDirection =
			let
				possibleDirs = possibleDirections (world_labyrinth world) (dt * speed) ghost :: [Direction]
			in
				if (obj_t ghost - lastDecision < 5) && (lastDir `elem` possibleDirs)
					then return lastDir
					else 
						uniform $ possibleDirs

possibleDirections :: Labyrinth -> DeltaT -> Object st -> [Direction]
possibleDirections lab deltaT obj =
	filter (\dir -> not $ willCollide lab (normalizeDir $ directionToSpeed $ dir) deltaT obj)
	allDirs

movePacman :: DeltaT -> World -> World
movePacman dt world =
	(
		set world_dbgInfo_l (DbgInf dbgText)
		.
		(over world_pacman_l $ moveObj (world_labyrinth world) dt dir speed)
	) world
	where
		dir :: Speed Float
		dir =
			normalizeDir $ directionsToSpeed $ world_userInput world
		speed = 2
		dbgText = concat $
			[ "userInput: ", show $ world_userInput world, "\n"
			, "pos: ", show (obj_pos $ world_pacman world), "\n"
			, "dir: ", show dir, "\n"
			]

normalizeDir :: Speed Float -> Speed Float
normalizeDir dir =
	if dir /= (0,0) then normalize $ dir else (0,0)

moveObj :: Labyrinth -> DeltaT -> Speed Float -> Float -> Object st -> Object st
moveObj labyrinth dt direction speed obj =
	let newPos = 
		if willCollide labyrinth direction (speed * dt) obj
		then obj_pos obj
		else
			pointInSizeF (fromIntegral $ mGetWidth labyrinth, fromIntegral $ mGetHeight labyrinth) $ -- torus
			(|+| (direction |* speed |* dt)) $
			obj_pos obj
	in
		(
			set obj_direction_l direction
			.
			over obj_t_l (+ dt)
			.
			set obj_pos_l newPos
		)
		obj

willCollide :: Labyrinth -> Speed Float -> Float -> Object st -> Bool
willCollide lab direction dt obj =
	any (willPointCollide lab direction dt) $
	rect (obj_pos obj) (obj_size obj)

willPointCollide :: Labyrinth -> Speed Float -> Float -> Pos Float -> Bool
willPointCollide lab dir dt pos =
	pointCollides lab $ 
	pos |+| (dir |* dt)

pointCollides :: Labyrinth -> Pos Float -> Bool
pointCollides lab =
	(==Wall) .
	(mGet `flip` lab) .
	swap .
	vecMap floor .
	pointInSizeF (fromIntegral $ mGetWidth lab, fromIntegral $ mGetHeight lab) -- torus
