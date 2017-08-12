{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GameData
import LevelGenerator
import Renderpipeline
import qualified Move
-- import SGData.Vector2D hiding ( (*|), (|*) )
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
windowSize = (800, 600)

main :: IO ()
main = play
	display
	bgColour
	framerate
	(genWorld 0)
	(renderWorld windowSize) -- calls renderWorld from Module Renderpipeline
	handleInput
	Move.moveWorld

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
		GameOver -> world
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
