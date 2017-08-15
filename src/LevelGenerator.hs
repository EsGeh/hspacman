{-# LANGUAGE RecordWildCards #-}
module LevelGenerator(
	WorldParams(..),
	genWorld
) where

import GameData --hiding(Direction)
import Vector2D
import RandomUtils
import Prelude hiding(Left,Right)
import Data.Tuple

import SGData.Matrix
import Control.Monad.Random
import Data.Maybe( fromJust )
import Data.List
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Lens.Micro.Platform


data WorldParams = WorldParams {
	worldParams_level :: Int,
	worldParams_size :: Size Int,
	worldParams_wallRatio :: Float,
	worldParams_ghostCount :: Int,
	worldParams_pacmanSpeed :: Float,
	worldParams_ghostsSpeed :: Float
}

{-
genWorld ::
	Int
	-> WorldParams
	-> World
genWorld seed params =
	genWorld' (mkStdGen seed) params

genWorld' ::
	StdGen
	-> WorldParams
	-> World
genWorld' rndGen params =
	let (world, newRndGen) =
		runRand `flip` rndGen $ genWorld'' rndGen params
	in set world_randomGen_l newRndGen world
-}

genWorld :: WorldParams -> State StdGen World
genWorld =
	withRandomGen . genWorld''

genWorld'' :: MonadRandom m => WorldParams -> m World
genWorld'' WorldParams{..} =
	do
		labyrinth <- genLabyrinth worldParams_size worldParams_wallRatio
		let allFreePositions =
			map swap $ 
			filter ((==Free) . flip mGet labyrinth) $
			mGetAllIndex labyrinth
		pacmanPos <-
			uniform $
			allFreePositions
		monsterPositions <-
			randomSubSet (worldParams_ghostCount) $
			filter ((>=3) . (distance $ vecMap fromIntegral $ pacmanPos) . vecMap fromIntegral) $
			allFreePositions
			
		{-
		let allFreePositions =
			map swap $ 
			filter ((==Free) . flip mGet labyrinth) $
			mGetAllIndex labyrinth
		startPositions@(pacmanPos : monsterPositions) <-
			randomSubSet (worldParams_ghostCount + 1) $ allFreePositions
		-}
		let dotPositions =
			allFreePositions \\ (pacmanPos : monsterPositions)
		return $
			World {
				world_statistics =  Statistics {
					world_level = worldParams_level,
					world_points = 0
				},
				world_labyrinth = labyrinth,
				world_pacman =
					(defObj (vecMap fromIntegral pacmanPos)),
				world_ghosts =
					map (defGhost . vecMap fromIntegral) monsterPositions,
				world_pacmanSpeed = worldParams_pacmanSpeed,
				world_ghostSpeed = worldParams_ghostsSpeed,
				world_dots =
					map (set obj_size_l dotSize) $
					map defObj $
					map ((|-| (dotSize |/ 2)) . (|+| (0.5,0.5))) $
					map (vecMap fromIntegral) $
					dotPositions,
				world_fruits= [],
				world_dbgInfo = DbgInf{ info = "" },
				world_userInput = [],
				world_t = 0
				--world_randomGen = rndGen
			}
	where
		dotSize = (0.2, 0.2)

randomSubSet :: (MonadRandom m, Eq a) => Int -> [a] -> m [a]
randomSubSet count list
	| count == 0 = return []
	| otherwise =
		do
			x <- uniform list
			fmap (x :) $ randomSubSet (count-1) (list\\[x])

-- create a labyrinth by spawning worms on a field that is massive in the beginning:
genLabyrinth ::
	MonadRandom m =>
	Size Int -> Float
	-> m Labyrinth
genLabyrinth (width,height) wallRatio = 
	randomTunnels wallRatio <=< firstTunnel $
	massiveField (width,height)

firstTunnel lab =
	do
		randomPos <-
			uniform $
			map swap $ 
			-- filter ((==Wall) . flip mGet lab) $
			mGetAllIndex lab
		let oneStepLeft = movePoint (width,height) randomPos (directionToSpeed Left)
		boreTunnel (wormBehaviourFirst Left 1) oneStepLeft Left
			=<<
			boreTunnel (wormBehaviourFirst Right 1) randomPos Right lab
	where
		(width,height) = (mGetWidth lab, mGetHeight lab)

-- a field with wall on all cells 
massiveField :: Size Int -> Labyrinth
massiveField (width,height) =
	fromJust $ mFromListRow $ replicate height line
	where
		line = replicate width Wall :: [Territory]

-- bore tunnels until the wall ratio has been reached:
randomTunnels :: (MonadRandom m) => Float -> Labyrinth -> m Labyrinth
randomTunnels wallRatio lab =
	if currentWallRatio <= wallRatio
	then return lab
	else randomTunnels wallRatio =<< randomTunnelsStep lab
	where
		currentWallRatio = (fromIntegral countWall) / (fromIntegral $ width*height)
		countWall = sum $ fmap fromEnum lab
		(width,height) = (mGetWidth lab, mGetHeight lab)

randomTunnelsStep :: MonadRandom m => Labyrinth -> m Labyrinth
randomTunnelsStep lab =
	do
		randomPos <-
			uniform $
			map swap $ 
			filter ((==Wall) . flip mGet lab) $
			mGetAllIndex lab
		let oneStepLeft = movePoint (width,height) randomPos (directionToSpeed Left)
		boreTunnel (wormBehaviour Left 0.5) oneStepLeft Left
			=<<
			boreTunnel (wormBehaviour Right 0.5) randomPos Right lab
	where
		(width,height) = (mGetWidth lab, mGetHeight lab)

boreTunnel ::
	MonadRandom m =>
	WormBehaviour m
	-> Pos Int
	-> Direction
	-> Labyrinth
	-> m Labyrinth
boreTunnel wormBeh pos0 favDir matr =
	evalStateT `flip`
		WormStatus{
			lastDir = favDir,
			history = [pos0]
		} $
		runWorm (wormStep wormBeh) $
		mSet (swap pos0) Free matr

-- runs a worm until its "dead" ( = returns "Nothing"):
runWorm :: Monad m => (Labyrinth -> MaybeT m Labyrinth) -> Labyrinth -> m Labyrinth
runWorm worm lab =
	runMaybeT (worm lab) >>= \mNewLabyrinth ->
	case mNewLabyrinth of
		Nothing -> return $ lab
		Just newLab -> runWorm worm $ newLab

data WormStatus = WormStatus {
	lastDir :: Direction,
	history :: [Pos Int]
	-- pos :: Pos Int
}

type WormBehaviour m =
	WormStatus -> Labyrinth -> m (Maybe Direction)

wormStep ::
	MonadRandom m =>
	WormBehaviour m
	-> Labyrinth
	-> MaybeT (StateT WormStatus m) Labyrinth
wormStep wormBeh matr =
	get >>= \wormStatus ->
	do
		newDir <- MaybeT $ lift $ wormBeh wormStatus matr
		(oldPos: _)  <- gets history
		let newPos = movePoint (mGetWidth matr, mGetHeight matr) oldPos . directionToSpeed $ newDir
		put $ WormStatus{
			lastDir = newDir,
			history = newPos : (history wormStatus)
			-- pos = newPos
		}
		return $
			mSet (swap newPos) Free matr

wormBehaviourFirst ::
	MonadRandom m =>
	Direction -> Rational
	-> WormBehaviour m
wormBehaviourFirst favDir prop WormStatus{..} matr =
	do
		rndDir <- randomDirS (favDir:(orthogonal favDir)) [(favDir, prop)]
		return $
			if
				(mGet (swap forwardPos) matr)/=Free &&
				(mGet (swap leftPos) matr)/=Free &&
				(mGet (swap rightPos) matr)/=Free 
			then Just $ rndDir
			else Nothing
	where
		[forwardPos, leftPos, rightPos] =
			map (movePoint (mGetWidth matr, mGetHeight matr) (head $ history) . directionToSpeed) $
				[lastDir, leftOf lastDir, rightOf lastDir]

wormBehaviour ::
	MonadRandom m =>
	Direction -> Rational
	-> WormBehaviour m
wormBehaviour favDir prop WormStatus{..} matr =
	do
		rndDir <- randomDirS (favDir:(orthogonal favDir)) [(favDir, prop)]
		return $
			if
				or $
				[ mGet (swap forwardPos) matr == Free && not (forwardPos `elem` history)
				, mGet (swap leftPos) matr == Free && not (leftPos `elem` history)
				, mGet (swap rightPos) matr == Free && not (rightPos `elem` history)
				]
			then Nothing
			else Just $ rndDir
	where
		[forwardPos, leftPos, rightPos] =
			map (movePoint (mGetWidth matr, mGetHeight matr) (head $ history) . directionToSpeed) $
				[lastDir, leftOf lastDir, rightOf lastDir]
