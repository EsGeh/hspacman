{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import GameData
import qualified LevelGenerator
import qualified Render
import qualified Move
import Vector2D

import Prelude hiding(Left,Right)

import Graphics.Gloss hiding(display)
import Graphics.Gloss.Interface.Pure.Game hiding(Up,Down)
import qualified Graphics.Gloss.Interface.Pure.Game as G

import Control.Monad.Random
import Lens.Micro.Platform
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
	do
		--let seed = 0
		seed <- getRandom
		play
			display
			bgColour
			framerate
			Menu
			(Render.render windowSize) -- calls renderWorld from Module Renderpipeline
			(handleInput seed)
			move

move :: DeltaT -> GameState -> GameState
move dt = \case
	Playing world -> Move.moveWorld dt world
	x -> x

handleInput :: Int -> Event -> GameState -> GameState
handleInput seed event state =
	case event of
		(EventKey key upOrDown _ _) -> case state of
			Playing world ->
				Playing $ over world_userInput_l `flip` world $
					case upOrDown of
						G.Down -> case key of
							Char 'w' -> addDir Up
							Char 's' -> addDir Down
							Char 'a' -> addDir Left
							Char 'd' -> addDir Right
							_ -> id
						_ -> id
						where
							-- addDir dir = const [dir]
							addDir dir =
								take 2 . (dir:)
								--(dir:) . (remDir $ opposite dir)
								-- ([dir] `union`) . (remDir $ opposite dir)
							remDir dir = filter (/=dir)
			Menu -> case upOrDown of
				G.Down -> case key of
					Char 's' ->
						Playing $ LevelGenerator.genWorld seed $ worldParamsFromDifficulty 1
					_ -> state
				_ -> state
			GameOver statistics ->
				case key of
					Char 's' ->
						Playing $ LevelGenerator.genWorld seed $ worldParamsFromDifficulty $ world_level statistics
					_ -> state
			Won statistics ->
				case key of
					Char 's' ->
						Playing $ LevelGenerator.genWorld seed $ worldParamsFromDifficulty $ (+1) $ world_level statistics
					_ -> state
		_ -> state

worldParamsFromDifficulty :: Int -> LevelGenerator.WorldParams
worldParamsFromDifficulty level =
	LevelGenerator.WorldParams{
		LevelGenerator.worldParams_level = level,
		LevelGenerator.worldParams_size = (worldSize, worldSize),
		LevelGenerator.worldParams_wallRatio = wallRatio,
		LevelGenerator.worldParams_ghostCount = floor $ ghostRatio * (fromIntegral $ worldSize*worldSize),
		LevelGenerator.worldParams_pacmanSpeed = speed,
		LevelGenerator.worldParams_ghostsSpeed = speed * 0.9
	}
	where
		speed =
			1.5 + (fromIntegral level) * 0.2
		worldSize = level*2 + 10
		wallRatio = 0.6
		ghostRatio = 1/60 :: Float

-- |changes the moving direction of the pacman
setPacDir :: World -> Speed Float -> World
setPacDir world dir = world { world_pacman = (world_pacman world) {obj_direction=dir}}
