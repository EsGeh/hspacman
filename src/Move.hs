{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE Rank2Types #-}
module Move where

import GameData
import Vector2D
import qualified Utils
import qualified Move.Collision as Coll

import Prelude hiding(Left,Right)

import Data.Maybe
import Data.List
import Control.Monad.Random
import Control.Monad.Writer
import Lens.Micro.Platform
--import Debug.Trace


moveWorld :: MonadRandom m => DeltaT -> World -> m GameState
moveWorld deltaT =
	fmap (
		maybeEndGame .
		(\(world, dbgText) -> setDbgText dbgText world)
	)
	.
	runWriterT
	.
	(return . movePacman deltaT <=< moveGhosts deltaT)
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
		setDbgText txt world =
				over world_dbgInfo_l (take 3 . (map $ head) . group . (txt:)) world

moveGhosts :: forall m . (MonadRandom m, MonadWriter String m) => DeltaT -> World -> m World
moveGhosts dt world = traverseOf world_ghosts_l (mapM $ moveGhost world dt) world

-- |ghosts move an "paths". When dest is reached, a new goal and path is chosen for the ghost
moveGhost :: forall m . (MonadRandom m, MonadWriter String m) => World -> DeltaT -> Ghost -> m Ghost
moveGhost world@World{..} dt ghost@Object{obj_state=GhostState{..}, ..} =
	case ghost_pathToDest of
		[]  ->
			let start = vecMap floor $ objCenter ghost
			in
				do
				newPath <-
					fmap (drop 1) $ -- skip the first entry, since this is where the ghost already is
					uniform $
					filter (not . null) $
					map (findPath world_labyrinth start) $
					possibleGhostDestinations world ghost
				-- traceM $ concat [ "start: ", show start, " path: ", show newPath ]
				moveGhost world dt $ set obj_state_l (GhostState newPath) ghost
		(nextDest:restPath) ->
			let
				dir =
					(vecMap fromIntegral nextDestFixed |+| (0.5,0.5))
					|-|
					objCenter ghost
				nextDestFixed =
					Utils.minimumOn (vec_length . vecMap fromIntegral . (|-| logicalPos) :: Pos Int -> Float) $
					[nextDest, nextDestInTorus]
				nextDestInTorus = pointInSize (labyrinth_size world_labyrinth) nextDest
				logicalPos = vecMap floor $ objCenter ghost
			in
				if
					((<0.05) . vec_length) $
					dir
				then 
					-- trace (concat [ "dir = ", show dir ]) $
					moveGhost world dt $ set obj_state_l (GhostState restPath) ghost
				else
					do
						-- tell $ show $ vecMap (roundTo 10) dir
						return $
							moveObjSimple torusSize (normalizeDir dir) (world_ghostSpeed * dt) ghost
	where
		torusSize = vecMap fromIntegral $ labyrinth_size world_labyrinth

roundTo :: Int -> Float -> Float
roundTo prec = (/fromIntegral prec) . fromInteger . floor . (*fromIntegral prec)

possibleGhostDestinations :: World -> Ghost -> [Pos Int]
possibleGhostDestinations World{..} ghost@Object{obj_state=GhostState{..}, ..} =
	let 
		logicalPos = vecMap floor $ objCenter ghost
	in
		map fst $
		filter ((==Free) . snd) $
		nextFields (4,4) logicalPos world_labyrinth

findPath :: Labyrinth -> Pos Int -> Pos Int -> [Pos Int]
findPath labyrinth start dest =
	let
		(xDir, yDir) = dest |-| start
		maybeNextStep =
			fmap (vecMap signum) $
			Utils.maximumOnSafe (vec_length . vecMap fromIntegral :: Speed Int -> Float) $
			filter openInDirection $
			filter (/=(0,0)) $
			[(xDir,0), (0,yDir)]
		-- |going 1 step into `dir` is possible?
		openInDirection :: Speed Int -> Bool
		openInDirection dir =
			labyrinth_get (pointInSize fieldSize $ start |+| (vecMap signum dir)) labyrinth == Free
		fieldSize = labyrinth_size labyrinth
	in
		-- trace (concat ["s=", show start, ", d=", show dest, " next=", show maybeNextStep, "\n"]) $
		maybe [] `flip` maybeNextStep $ \nextStep ->
			let nextPos = start |+| nextStep
			in
				if nextPos == dest
				then start : [dest]
				else
					start : findPath labyrinth nextPos dest

possibleDirectionsClever :: Labyrinth -> Object st -> [Direction]
possibleDirectionsClever lab obj =
	filter (\dir -> not $ Coll.willCollideWithLabyrinth lab (directionToSpeed $ dir) 0.4 obj)
	allDirs

possibleDirections :: Labyrinth -> DeltaT -> Object st -> [Direction]
possibleDirections lab deltaT obj =
	filter (\dir -> not $ Coll.willCollideWithLabyrinth lab (normalizeDir $ directionToSpeed $ dir) deltaT obj)
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
		torusSize = vecMap fromIntegral $ labyrinth_size labyrinth
		labyrinth = world_labyrinth world

maybeEatDot :: World -> World
maybeEatDot world =
	let 
		pacman_pos = obj_pos $ world_pacman world
		pacman_size = obj_size $ world_pacman world
	in
		(\newWorld -> if world_dots world /= world_dots newWorld then over (world_statistics_l . world_points_l) (+1) newWorld else newWorld) $
		over world_dots_l `flip` world $ filter $ \dot ->
		not $ Coll.rectCollidesRect (pacman_pos, pacman_size) (obj_pos dot, obj_size dot)

isWon :: World -> Bool
isWon world = (sum $ map (const 1) $ world_dots world) == (0 :: Int)

isGameOver :: World -> Bool
isGameOver world =
	let 
		pacman_pos = obj_pos $ world_pacman world
		pacman_size = obj_size $ world_pacman world
	in
		any (Coll.rectCollidesRect (pacman_pos, pacman_size)) $
		map `flip` world_ghosts world $
		\ghost -> (obj_pos ghost, obj_size ghost)

moveObjSimple :: Size Float -> Speed Float -> Float -> Object st -> Object st
moveObjSimple torusSize direction speed obj =
	set obj_direction_l direction $
	over obj_pos_l `flip` obj $ 
	pointInSizeF torusSize . -- torus
	(|+| (direction |* speed ))

normalizeDir :: Speed Float -> Speed Float
normalizeDir dir =
	if dir /= (0,0) then normalize $ dir else (0,0)
