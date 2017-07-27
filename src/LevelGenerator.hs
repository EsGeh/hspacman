{-# LANGUAGE RecordWildCards #-}
module LevelGenerator(
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

genWorld :: Int -> World
genWorld seed =
	--flip evalRand (mkStdGen seed) $
	genWorld' (mkStdGen seed) (20,20) 0.4

genWorld' ::
	StdGen ->
	Size Int -> Float
	-> World
genWorld' rndGen worldSize wallRatio =
	let ((labyrinth, (pacmanPos:monsterPositions)), newRndGen) = runRand `flip` rndGen $
		do
			labyrinth <- genLabyrinth worldSize wallRatio
			startPositions <-
				randomSubSet 5 $
				map swap $ 
				filter ((==Free) . flip mGet labyrinth) $
				mGetAllIndex labyrinth
			return (labyrinth, startPositions)
	in
			World {
				world_uiState = Menu,
				world_level = 1,
				world_points = 0,
				world_labyrinth = labyrinth,
				world_pacman =
					(defObj (vecMap fromIntegral pacmanPos)),
				world_ghosts =
					map (defGhost . vecMap fromIntegral) monsterPositions,
				world_dots = [],
				world_fruits= [],
				world_dbgInfo = DbgInf{ info = "" },
				world_userInput = [],
				world_randomGen = newRndGen
			}

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
	randomTunnels wallRatio $
	massiveField (width,height)

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
	else
		do
			randomPos <- uniform $ map swap $ mGetAllIndex lab
			let oneStepLeft = movePoint (width,height) randomPos (directionToSpeed Left)
			lab' <-
				boreTunnel oneStepLeft Left
				=<<
				boreTunnel randomPos Right lab
			randomTunnels wallRatio lab' -- <- recursion !
	where
		currentWallRatio = (fromIntegral countWall) / (fromIntegral $ width*height)
		countWall = sum $ fmap fromEnum lab
		(width,height) = (mGetWidth lab, mGetHeight lab)

boreTunnel ::
	MonadRandom m =>
	Size Int
	-> Direction
	-> Labyrinth
	-> m Labyrinth
boreTunnel pos0 favDir matr =
	evalStateT `flip`
		WormStatus{
			lastDir = favDir,
			pos = pos0
		} $
		runWorm (wormStep favDir 0.95) $
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
	pos :: Pos Int
}

wormStep ::
	MonadRandom m =>
	Direction -> Rational
	-> Labyrinth
	-> MaybeT (StateT WormStatus m) Labyrinth
wormStep favDir prop matr =
	get >>= \wormStatus ->
	do
		newDir <- MaybeT $ lift $ wormBehaviour favDir prop wormStatus matr
		oldPos <- gets pos
		let newPos = movePoint (mGetWidth matr, mGetHeight matr) oldPos . directionToSpeed $ newDir
		put $ WormStatus{
			lastDir = newDir,
			pos = newPos
		}
		return $
			mSet (swap newPos) Free matr

wormBehaviour ::
	MonadRandom m =>
	Direction -> Rational
	-> WormStatus
	-> Labyrinth
	-> m (Maybe Direction)
wormBehaviour favDir prop WormStatus{..} matr =
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
			map (movePoint (mGetWidth matr, mGetHeight matr) pos . directionToSpeed) $
				[lastDir, leftOf lastDir, rightOf lastDir]

-- tests if a position is inside a given area:
{-
inBox :: Area -> Pos -> Bool
inBox (posBox,sizeBox) pos = (pos `vecGOE` posBox) && (pos `vecSOE` (posBox |+| sizeBox))
	where
		vecGOE l r = (vecX l >= vecX r) && (vecY l >= vecY r)
		vecSOE l r = (vecX l <= vecX r) && (vecY l <= vecY r)
-}
