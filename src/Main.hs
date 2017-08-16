{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import GameData hiding( Left, Right )
import qualified GameData as Dir ( Direction( Up, Down, Left, Right ) )
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
import Data.Either
import System.Random( getStdGen )
import Codec.BMP( readBMP, BMP )

imgPath = "res"

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
		imgResources <- loadImageResources
		play
			display
			bgColour
			framerate
			(Menu, startRandomGen)
			(\(st, rndGen) -> evalRand `flip` rndGen $ Render.render imgResources windowSize st)
			(\event x -> runState `flip` (snd x) $ handleInput event $ fst x)
			(\dt (st, rndGen) -> runState `flip` rndGen $ move dt st)

loadImageResources :: IO Render.ImageResources
loadImageResources =
	do
		imgRes_wallTile <- fmap (either (error . show) id) $ readBMP $ imgPath ++ "/wall_tile.bmp"
		return $ Render.ImageResources {
			..
		}

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
							Char 'a' -> addDir Dir.Left
							Char 'd' -> addDir Dir.Right
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
