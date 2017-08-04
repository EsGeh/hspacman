{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
	(movePacman deltaT) .
	(moveGhosts deltaT)

moveGhosts :: DeltaT -> World -> World
moveGhosts dt world =
	let (newWorld, newRnd) =
		runRand `flip` (world_randomGen world) $
		world_mapToGhostsM (mapM $ moveGhost world dt) world
	in
		newWorld{ world_randomGen = newRnd }

moveGhost :: MonadRandom m => World -> DeltaT -> Ghost -> m Ghost
moveGhost world dt =
	fmap (moveCharacter dt world) .
	ghostBehaviour world dt

ghostBehaviour :: forall m . MonadRandom m => World -> DeltaT -> Ghost -> m Ghost
ghostBehaviour world dt ghost@Object{ obj_state = GhostState { ghost_dir_history }, .. } =
	let
		[(lastDir, lastDecision)] = ghost_dir_history
		possibleDirs = possibleDirections (world_labyrinth world) dt ghost :: [Direction]
	in
	do
		(newDir, newLastDecision) <-
			if (obj_t - lastDecision < 5) && (lastDir `elem` possibleDirs)
				then return (lastDir, lastDecision)
				else 
					fmap (,obj_t) $
					uniform $ possibleDirs
			:: m (Direction, Float)
		{-
		(newDir, newLastDecision) <-
			if obj_t - lastDecision > 5
			then
				fmap (,obj_t) $
				--uniform $ allDirs
				uniform $
				possibleDirections (world_labyrinth world) dt ghost
			else return (lastDir, lastDecision)
		-}
		return $ ghost{
			obj_direction = speed *| directionToSpeed newDir,
			obj_state = GhostState{ ghost_dir_history= [(newDir, newLastDecision)] }
		}
	where
		speed :: Float
		speed = 1

movePacman :: DeltaT -> World -> World
movePacman dt world@World{ world_pacman=pacMan } =
	world {
		world_pacman = moveCharacter dt world $ setDirection pacMan,
		world_dbgInfo = DbgInf dbgText
	}
	where
		setDirection :: Pacman -> Pacman
		setDirection obj =
			obj{
				obj_direction =
					speed *| (vecMap fromInteger $ directionsToSpeed $ world_userInput world)
			}
		dbgText = concat $
			[ "userInput: ", show $ world_userInput world, "\n"
			, "pos: ", show (obj_pos pacMan), "\n"
			, "setDirection: ", show (setDirection pacMan), "\n"
			]
		speed = 2

moveCharacter :: DeltaT -> World -> Object st -> Object st
moveCharacter dt world obj =
	obj{
		obj_pos = newPos,
		obj_t = (obj_t obj + dt)
	}
	where
		newPos =
			if (willCollide (world_labyrinth world) dt obj)
			then obj_pos obj
			else (pointInSizeF labSize $ obj_pos obj |+| (obj_direction obj) |* dt) -- pointInSize: torus
				where
					labSize = vecMap fromIntegral (mGetWidth lab, mGetHeight lab)
					lab = world_labyrinth world

possibleDirections :: Labyrinth -> DeltaT -> Object st -> [Direction]
possibleDirections lab deltaT obj =
	filter (not . willCollide lab deltaT . (\x -> obj{ obj_direction= x }) . directionToSpeed) $
	allDirs

willCollide :: Labyrinth -> DeltaT -> Object st -> Bool
willCollide lab deltaT obj =
	any (willPointCollide lab deltaT $ obj_direction obj) $
	rect (obj_pos obj) (obj_size obj)

willPointCollide :: Labyrinth -> Float -> Speed Float -> Pos Float -> Bool
willPointCollide lab deltaT dir oldPos =
	(==Wall) $
	mGet `flip` lab $
	(\x -> if fst x<0 || snd x <0 || snd x > mGetWidth lab -1 || fst x > mGetWidth lab -1 then error $ concat ["index: ", show x, " oldPos: ", show oldPos, " nextPos: ", show nextPos] else x) $
	calcMatrIndex $
	nextPos
	where
		calcMatrIndex :: Pos Float -> MatrIndex
		calcMatrIndex pos =
			swap $
			vecMap floor $
			pointInSizeF (fromIntegral $ mGetWidth lab, fromIntegral $ mGetHeight lab) pos -- torus
		nextPos :: Pos Float
		nextPos = oldPos |+| (dir |* deltaT)
