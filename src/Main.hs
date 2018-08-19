{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import GameData hiding( Up, Down, Left, Right )
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
import Control.Monad.State.Strict
import Lens.Micro.Platform
import System.Random( getStdGen )


imgPath :: String
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
framerate = 60

type ActualState = (GameState, StdGen)

main :: IO ()
main =
	do
		startRandomGen <- getStdGen
		imgResources <- loadImageResources
		--return ()
		play
			display
			bgColour
			framerate
			(Menu, startRandomGen)
			(\(st, rndGen) -> evalRand `flip` rndGen $ Render.render imgResources windowSize st)
			(\event x -> runState `flip` (snd x) $ handleInput event $ fst x)
			(\dt (st, rndGen) -> runState `flip` rndGen $ move dt st)

withRandomGen ::
	-- (forall m . MonadRandom m => m b)
		Rand StdGen b
	-> State StdGen b
withRandomGen x =
	do
		rndGen <- get :: State StdGen StdGen
		let (res, newState) = runRand x rndGen
		put newState
		return res

loadImageResources :: IO Render.ImageResources
loadImageResources =
	do
		imgRes_wallTile <- loadBMP $ imgPath ++ "/wall_tile.bmp"
		imgRes_floorTile <- loadBMP $ imgPath ++ "/floor_tile.bmp"
		imgRes_ghost <- loadBMP $ imgPath ++ "/ghost.bmp"
		imgRes_pacman <- Render.loadSpriteSheet (32,32) $ imgPath ++ "/pacman.bmp"
		imgRes_font <- Render.loadBitmapFont (24,32) $ imgPath ++ "/outline_24x32.bmp"
		return $ Render.ImageResources {
			..
		}

move :: DeltaT -> GameState -> State StdGen GameState
move dt = \case
	Playing world ->
		withRandomGen $ Move.moveWorld dt world
	x -> return $ x

handleInput :: Event -> GameState -> State StdGen GameState
handleInput event st =
	case event of
		(EventKey key upOrDown _ _) -> case st of
			Playing world ->
				return $ Playing $ over world_userInput_l `flip` world $
					case upOrDown of
						G.Down -> case key of
							Char 'w' -> addDir Dir.Up
							Char 's' -> addDir Dir.Down
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
					SpecialKey KeySpace ->
						fmap Playing $ withRandomGen $ LevelGenerator.genWorld $ worldParamsFromDifficulty 1
					_ -> return st
				_ -> return st
			GameOver statistics ->
				case key of
					SpecialKey KeySpace ->
						fmap Playing $ withRandomGen $ LevelGenerator.genWorld $ worldParamsFromDifficulty $ world_level statistics
					_ -> return st
			Won statistics ->
				case key of
					SpecialKey KeySpace ->
						fmap Playing $ withRandomGen $ LevelGenerator.genWorld $ worldParamsFromDifficulty $ (+1) $ world_level statistics
					_ -> return st
		_ -> return st

worldParamsFromDifficulty :: Int -> LevelGenerator.WorldParams
--worldParamsFromDifficulty :: Monad m => Int -> m LevelGenerator.WorldParams
worldParamsFromDifficulty level =
	LevelGenerator.WorldParams{
		LevelGenerator.worldParams_level = level,
		LevelGenerator.worldParams_size =
			(worldSize*2, worldSize),
			--(floor $ fromIntegral worldSize*3/2, worldSize),
		LevelGenerator.worldParams_gridStep =
			(2,3),
		LevelGenerator.worldParams_ghostCount =
			floor $ ghostRatio * (fromIntegral $ worldSize*worldSize),
		LevelGenerator.worldParams_pacmanSpeed = speed,
		LevelGenerator.worldParams_ghostsSpeed = speed * 0.9
	}
	where
		speed =
			1.5 + (fromIntegral level) * 0.2
		worldSize = level*2 + 5
		ghostRatio = 1/60 :: Float

-- |changes the moving direction of the pacman
setPacDir :: World -> Speed Float -> World
setPacDir world dir = world { world_pacman = (world_pacman world) {obj_direction=dir}}
