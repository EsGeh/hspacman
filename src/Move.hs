{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE Rank2Types #-}
module Move where

import GameData
import Vector2D
import SGData.Matrix

import Prelude hiding(Left,Right)

import Data.Tuple
import Data.Maybe
import Control.Monad.Random

import Lens.Micro.Platform


moveWorld :: MonadRandom m => DeltaT -> World -> m GameState
moveWorld deltaT =
	fmap (
		maybeEndGame
		.
		setDbgText 
		.
		movePacman deltaT
	)
	.
	(moveGhosts deltaT)
	.
	over world_t_l (+ deltaT)
	where
		maybeEndGame world =
			if isWon world
				then Won (world_statistics world)
				else
					if isGameOver world
					then GameOver $ world_statistics world
					else Playing world
		setDbgText world =
			set world_dbgInfo_l (DbgInf dbgText) world
			where
				dbgText = concat $
					[ "userInput: ", show $ world_userInput world, "\n"
					, "pos: ", show (obj_pos $ world_pacman world), "\n"
					]

{-
moveGhosts :: StdGen -> DeltaT -> World -> State World
moveGhosts rndGen dt world =
	let (newWorld, newRnd) =
		runRand `flip` (world_randomGen world) $
		traverseOf world_ghosts_l (mapM $ moveGhost world dt) world
	in
		newWorld{ world_randomGen = newRnd }
-}

moveGhosts :: forall m . MonadRandom m => DeltaT -> World -> m World
moveGhosts dt world = traverseOf world_ghosts_l (mapM $ moveGhost world dt) world

moveGhost :: forall m . MonadRandom m => World -> DeltaT -> Ghost -> m Ghost
moveGhost world dt ghost =
	do
		direction <- calcDirection :: m Direction
		let newLastDecision =
			if direction /= lastDir then world_t world else lastDecision :: Float
		return $
			set (obj_state_l . ghost_dir_history_l) [(direction, newLastDecision)] $
			moveObjSimple torusSize ((normalizeDir . directionToSpeed) $ direction) (speed * dt) ghost
			--moveObjInsideWalls (world_labyrinth world) dt ((normalizeDir . directionToSpeed) $ direction) speed ghost
	where
		speed :: Float
		speed = world_ghostSpeed world
		[(lastDir, lastDecision)] = ghost_dir_history $ obj_state $ ghost
		calcDirection :: m Direction
		calcDirection =
			let
				possibleDirs = possibleDirections labyrinth (dt * speed) ghost :: [Direction]
			in
				if (world_t world - lastDecision < 5) && (lastDir `elem` possibleDirs)
					then return lastDir
					else 
						uniform $ possibleDirs
		torusSize = (fromIntegral $ mGetWidth labyrinth, fromIntegral $ mGetHeight labyrinth)
		labyrinth = world_labyrinth world

possibleDirections :: Labyrinth -> DeltaT -> Object st -> [Direction]
possibleDirections lab deltaT obj =
	filter (\dir -> not $ willCollideWithLabyrinth lab (normalizeDir $ directionToSpeed $ dir) deltaT obj)
	allDirs

movePacman :: DeltaT -> World -> World
movePacman dt world =
	maybeEatDot $
	fromMaybe world $
	do
		primaryDir <- listToMaybe userInput :: Maybe Direction
		if primaryDir `elem` possibleDirs
			then return $ over world_pacman_l `flip` world $ moveObjSimple torusSize (normalizeDir $ directionToSpeed primaryDir) (world_pacmanSpeed world * dt)
			else
				do
					secondaryDir <- listToMaybe $ tail userInput
					if secondaryDir `elem` possibleDirs
						then return $
							--over world_userInput_l tail $
							over world_pacman_l `flip` world $
							moveObjSimple torusSize (normalizeDir $ directionToSpeed secondaryDir) (world_pacmanSpeed world * dt)
						else
							Nothing
	where
		possibleDirs =
			possibleDirections (world_labyrinth world) (dt * speed) (world_pacman world) :: [Direction]
		userInput = world_userInput world :: [Direction]
		speed =
			world_pacmanSpeed world
		torusSize = (fromIntegral $ mGetWidth labyrinth, fromIntegral $ mGetHeight labyrinth)
		labyrinth = world_labyrinth world
		-- pacman = world_pacman world

maybeEatDot :: World -> World
maybeEatDot world =
	let 
		pacman_pos = obj_pos $ world_pacman world
		pacman_size = obj_size $ world_pacman world
	in
		(\newWorld -> if world_dots world /= world_dots newWorld then over (world_statistics_l . world_points_l) (+1) newWorld else newWorld) $
		over world_dots_l `flip` world $ filter $ \dot ->
		not $ rectCollidesRect (pacman_pos, pacman_size) (obj_pos dot, obj_size dot)

isWon :: World -> Bool
isWon world = (sum $ map (const 1) $ world_dots world) == (0 :: Int)

isGameOver :: World -> Bool
isGameOver world =
	let 
		pacman_pos = obj_pos $ world_pacman world
		pacman_size = obj_size $ world_pacman world
	in
		any (rectCollidesRect (pacman_pos, pacman_size)) $
		map `flip` world_ghosts world $
		\ghost -> (obj_pos ghost, obj_size ghost)

{-
moveObjInsideWallsMaybe :: Labyrinth -> DeltaT -> Speed Float -> Float -> Object st -> Maybe (Object st)
moveObjInsideWallsMaybe labyrinth dt direction speed obj =
	if willCollideWithLabyrinth labyrinth direction (speed * dt) obj
	then Nothing
	else return $ moveObjSimple torusSize direction (speed * dt) obj
	where
		torusSize = (fromIntegral $ mGetWidth labyrinth, fromIntegral $ mGetHeight labyrinth)
-}

moveObjSimple :: Size Float -> Speed Float -> Float -> Object st -> Object st
moveObjSimple torusSize direction speed obj =
	set obj_direction_l direction $
	over obj_pos_l `flip` obj $ 
	pointInSizeF torusSize . -- torus
	(|+| (direction |* speed ))

{-
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
	)
	obj
-}

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
