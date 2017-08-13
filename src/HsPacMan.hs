{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import GameData
import qualified LevelGenerator
import qualified Renderpipeline
import qualified Move
import Vector2D

import Prelude hiding(Left,Right)

import Graphics.Gloss hiding(display)
import Graphics.Gloss.Interface.Pure.Game hiding(Up,Down)
import qualified Graphics.Gloss.Interface.Pure.Game as G

import Data.List


windowTitle :: String
windowTitle = "hsPacMan"
windowPos :: Pos Float
windowPos = (100, 100) 
windowSize :: Pos Float
windowSize = (1000, 800)

display :: Display
display = InWindow windowTitle (vecMap floor windowSize) (vecMap floor windowPos)

bgColour :: Color
bgColour = black

framerate :: Int
framerate = 40

main :: IO ()
main =
	play
		display
		bgColour
		framerate
		Menu
		(Renderpipeline.render windowSize) -- calls renderWorld from Module Renderpipeline
		handleInput
		move

move :: DeltaT -> GameState -> GameState
move dt = \case
	Playing world -> Move.moveWorld dt world
	x -> x

handleInput :: Event -> GameState -> GameState
handleInput event state =
	case event of
		(EventKey key upOrDown _ _) -> case state of
			Playing world ->
				Playing $ case upOrDown of
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
			Menu -> case upOrDown of
				G.Down -> case key of
					Char 's' ->
						Playing $ LevelGenerator.genWorld 8 $ worldParamsFromDifficulty 1
					_ -> state
				_ -> state
			GameOver statistics ->
				case key of
					Char 's' ->
						Playing $ LevelGenerator.genWorld 8 $ worldParamsFromDifficulty $ world_level statistics
					_ -> state
			Won statistics ->
				case key of
					Char 's' ->
						Playing $ LevelGenerator.genWorld 8 $ worldParamsFromDifficulty $ (+1) $ world_level statistics
					_ -> state
		_ -> state

worldParamsFromDifficulty :: Int -> LevelGenerator.WorldParams
worldParamsFromDifficulty level =
	LevelGenerator.WorldParams{
		LevelGenerator.worldParams_level = level,
		LevelGenerator.worldParams_size = (worldSize, worldSize),
		LevelGenerator.worldParams_wallRatio = wallRatio,
		LevelGenerator.worldParams_ghostCount = level + 1,
		LevelGenerator.worldParams_pacmanSpeed =
			1 + (fromIntegral level) * 0.2,
		LevelGenerator.worldParams_ghostsSpeed =
			1 + ((fromIntegral level) - 1) * 0.2
	}
	where
		worldSize = level*2 + 5
		wallRatio = 0.6

-- |changes the moving direction of the pacman
setPacDir :: World -> Speed Float -> World
setPacDir world dir = world { world_pacman = (world_pacman world) {obj_direction=dir}}
