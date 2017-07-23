module Main where

import GameData
import LevelGenerator
import Renderpipeline
import SGData.Vector2D hiding ( (*|), (|*) )
import Vector2D ( (*|), (|*) )
--import Vector2D
import SGData.Matrix
import RandomUtils

import Prelude hiding(Left,Right)

import Graphics.Gloss hiding(display)
import Graphics.Gloss.Interface.Pure.Game hiding(Up,Down)
import qualified Graphics.Gloss.Interface.Pure.Game as G

-- import System.Random
import Control.Monad.Random

import Data.Tuple
import Data.List


windowTitle :: String
windowTitle = "hsPacMan"
windowPos :: PosOnScreen
windowPos = (100, 100) 
windowSize :: SizeOnScreen
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
	(movePacman deltaT)
	-- (moveGhosts deltaT)

{-
moveGhosts :: DeltaT -> World -> World
moveGhosts dt world0 =
	world0{
		world_ghosts =
			map (moveCharacter dt world0 . setDirection world0) $ world_ghosts world0,
		world_dbgInfo=
			DbgInf{
				info= "" -- show $ possibleDirections (world_labyrinth world0) dt (head $ world_ghosts world0 )
			}
	}
	where
		--monsterSpeed = (fromIntegral $ level world)
		-- this function is the ai for the ghosts
		setDirection :: World -> Ghost -> Ghost
		setDirection world ghost = ghost{
			obj_direction= obj_direction $ world_pacman world,
			--obj_direction= vecMap fromIntegral $ directionsToSpeed [newDir],
			obj_state = obj_state ghost
			--obj_state = GhostState{ rndState=newRndState }
		}
			where
				(newDir,newRndState) = runRand rndDir (rndState $ obj_state ghost)
				rndDir = randomDirS possibleDirs [(head $ speedToDirection $ obj_direction ghost,0.98)]
				possibleDirs = possibleDirections (world_labyrinth world) dt ghost
				pacManObj= world_pacman world
-}


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
					speed *| (vecMap fromIntegral $ directionsToSpeed $ world_userInput world)
			}
		dbgText = concat ["userInput: ", show $ world_userInput world ]
			{-"pos pacMan: " ++ (show $ vecMap floor $ pos pacMan) ++ "\n" ++
			"pos pacMan exact: " ++ (show $ pos pacMan) ++ "\n" {-++
			"possibleDirs: " ++ show possibleDirs-} -}
		speed = 2

moveCharacter :: DeltaT -> World -> Object st -> Object st
moveCharacter dt world obj = obj{ obj_pos=newPos, obj_t= (obj_t obj + dt) }
	where
		
		newPos = if (willCollide (world_labyrinth world) dt obj)
			then obj_pos obj
			else (pointInSizeF labSize $ obj_pos obj |+| (obj_direction obj) |* dt) -- pointInSize: torus
				where
					labSize = vecMap fromIntegral (mGetWidth lab -1,mGetHeight lab -1)
					lab = world_labyrinth world

possibleDirections :: Labyrinth -> DeltaT -> Object st -> [Direction]
possibleDirections lab deltaT obj =
	filter (not . willCollide lab deltaT . (\x -> obj{ obj_direction= x }) . directionToSpeed) $
	allDirs

willCollide :: Labyrinth -> DeltaT -> Object st -> Bool
willCollide lab deltaT obj =
	or $
	map (willPointCollide lab deltaT (obj_direction obj)) $
	[ p , p |+| (0,h), p |+| (w,h), p |+| (w,0) ]
	where
		p = obj_pos obj
		(w,h) = obj_size obj

willPointCollide :: Labyrinth -> Float -> Speed Float -> Pos Float -> Bool
willPointCollide lab deltaT dir oldPos =
	(==Wall) $ mGet (calcMatrIndex nextPos) lab 
	where
		calcMatrIndex :: Pos Float -> MatrIndex
		calcMatrIndex pos = swap $
			vecMap floor $
			pointInSizeF (fromIntegral $ mGetWidth lab -1, fromIntegral $ mGetHeight lab -1) pos -- torus
		nextPos :: Pos Float
		nextPos = (oldPos |+| dir |* deltaT)
