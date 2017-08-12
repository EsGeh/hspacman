{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Move where

import GameData
import Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)

import Data.Tuple
import Control.Monad.Random

import Lens.Micro.Platform


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
			moveObjInsideWalls (world_labyrinth world) dt ((normalizeDir . directionToSpeed) $ direction) speed ghost
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
	filter (\dir -> not $ willCollideWithLabyrinth lab (normalizeDir $ directionToSpeed $ dir) deltaT obj)
	allDirs

movePacman :: DeltaT -> World -> World
movePacman dt world =
	(
		if isGameOver then set world_uiState_l GameOver else id
		.
		maybeEatDot
		.
		set world_dbgInfo_l (DbgInf dbgText)
		.
		(over world_pacman_l $ moveObjInsideWalls (world_labyrinth world) dt dir speed)
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
		isGameOver =
			let 
				pacman_pos = obj_pos $ world_pacman world
				pacman_size = obj_size $ world_pacman world
			in
				any (rectCollidesRect (pacman_pos, pacman_size)) $
				map `flip` world_ghosts world $
				\ghost -> (obj_pos ghost, obj_size ghost)
		maybeEatDot world =
			let 
				pacman_pos = obj_pos $ world_pacman world
				pacman_size = obj_size $ world_pacman world
			in
				over world_dots_l `flip` world $ filter $ \dot ->
				not $ rectCollidesRect (pacman_pos, pacman_size) (obj_pos dot, obj_size dot)
			
moveObjInsideWalls :: Labyrinth -> DeltaT -> Speed Float -> Float -> Object st -> Object st
moveObjInsideWalls labyrinth dt direction speed obj =
	(
		if willCollideWithLabyrinth labyrinth direction (speed * dt) obj
			then id
			else (over obj_pos_l $ 
				pointInSizeF (fromIntegral $ mGetWidth labyrinth, fromIntegral $ mGetHeight labyrinth) . -- torus
				(|+| (direction |* speed |* dt))
			)
		.
		set obj_direction_l direction
		.
		over obj_t_l (+ dt)
	)
	obj

normalizeDir :: Speed Float -> Speed Float
normalizeDir dir =
	if dir /= (0,0) then normalize $ dir else (0,0)

willCollideWithLabyrinth :: Labyrinth -> Speed Float -> Float -> Object st -> Bool
willCollideWithLabyrinth lab direction dt obj =
	any (willPointCollideWithLabyrinth lab direction dt) $
	rect (obj_pos obj) (obj_size obj)

willPointCollideWithLabyrinth :: Labyrinth -> Speed Float -> Float -> Pos Float -> Bool
willPointCollideWithLabyrinth lab dir dt pos =
	pointCollidesWithLabyrinth lab $ 
	pos |+| (dir |* dt)

pointCollidesWithLabyrinth :: Labyrinth -> Pos Float -> Bool
pointCollidesWithLabyrinth lab =
	(==Wall) .
	(mGet `flip` lab) .
	swap .
	vecMap floor .
	pointInSizeF (fromIntegral $ mGetWidth lab, fromIntegral $ mGetHeight lab) -- torus

rectCollidesRect :: (Pos Float, Size Float) -> (Pos Float, Size Float) -> Bool
rectCollidesRect r1 r2 =
	rectCollidesRect' r1 r2 || rectCollidesRect' r2 r1

rectCollidesRect' :: (Pos Float, Size Float) -> (Pos Float, Size Float) -> Bool
rectCollidesRect' (pos1, size1) (pos2, size2) =
	any (pointCollidesRect (pos2, size2)) $
	rect pos1 size1

pointCollidesRect :: (Pos Float, Pos Float) -> Pos Float -> Bool
pointCollidesRect (rPos, rSize) p =
	and $
	[ fst p >= fst rPos
	, snd p >= snd rPos
	, fst p <= fst (rPos |+| rSize)
	, snd p <= snd (rPos |+| rSize)
	]
