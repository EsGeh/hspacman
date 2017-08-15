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
import Control.Monad.State
import Lens.Micro.Platform
import Data.List
import System.Random( getStdGen )


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
		startRandomGen <- getStdGen
		play
			display
			bgColour
			framerate
			(Menu, startRandomGen)
			(\(st, rndGen) -> evalRand `flip` rndGen $ Render.render windowSize st)
			(\event x -> runState `flip` (snd x) $ handleInput event $ fst x)
			(\dt (st, rndGen) -> runState `flip` rndGen $ move dt st)

move :: DeltaT -> GameState -> State StdGen GameState
move dt = \case
	Playing world ->
		withRandomGen $ Move.moveWorld dt world
	x -> return $ x

type ActualState = (GameState, StdGen)

handleInput :: Event -> GameState -> State StdGen GameState
handleInput event state =
	case event of
		(EventKey key upOrDown _ _) -> case state of
			Playing world ->
				return $ Playing $ over world_userInput_l `flip` world $
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
								take 2 . (dir:) . (remDir $ opposite dir)
								-- ([dir] `union`) . (remDir $ opposite dir)
							remDir dir = filter (/=dir)
			Menu -> case upOrDown of
				G.Down -> case key of
					Char 's' ->
						fmap Playing $ LevelGenerator.genWorld $ worldParamsFromDifficulty 1
					_ -> return state
				_ -> return state
			GameOver statistics ->
				case key of
					Char 's' ->
						fmap Playing $ LevelGenerator.genWorld $ worldParamsFromDifficulty $ world_level statistics
					_ -> return state
			Won statistics ->
				case key of
					Char 's' ->
						fmap Playing $ LevelGenerator.genWorld $ worldParamsFromDifficulty $ (+1) $ world_level statistics
					_ -> return state
		_ -> return state

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
		worldSize = level*2 + 5
		wallRatio = 0.6
		ghostRatio = 1/60 :: Float

-- |changes the moving direction of the pacman
setPacDir :: World -> Speed Float -> World
setPacDir world dir = world { world_pacman = (world_pacman world) {obj_direction=dir}}
