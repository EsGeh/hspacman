module LevelGenerator(
	genWorld
	--genLabyrinth
) where

import GameData --hiding(Direction)
--import SGData.Vector2D
import RandomUtils
import Prelude hiding(Left,Right)
import Data.Tuple
import qualified Data.Foldable as F
import Control.Monad.Identity

import SGData.Matrix
import Control.Monad.Random
import Data.Maybe( fromJust )

genWorld seed =
	flip evalRand (mkStdGen seed) $
	genWorld' (20,20) 0.4

genWorld' ::
	RandomGen g =>
	Size Int -> Float
	-> RandT g Identity World
genWorld' worldSize wallRatio =
	do
		labyrinth <- genLabyrinth worldSize wallRatio
		pacmanPos <-
			return (0,0)
		return $
			World {
				world_uiState = Menu,
				world_level = 1,
				world_points = 0,
				world_labyrinth = labyrinth,
				world_pacman =
					(defObj pacmanPos){
						obj_size = pacManSize,
						obj_direction = (0,0),
						obj_t = 0
					},
				world_ghosts = ghosts,
				world_dots = [],
				world_fruits= [],
				world_dbgInfo = DbgInf{ info = "" },
				world_userInput = []
			}
	where
		pacManSize =
			(1,1)
			--(0.7,0.7)
		ghosts = []
			--[ Object{ obj_pos=(0,0), obj_size=pacManSize, obj_direction=(0,0), obj_t=0, obj_state=GhostState { rndState= mkStdGen seed } }]


-- create a labyrinth by spawning worms on a field that is massive in the beginning:
genLabyrinth ::
	RandomGen g =>
	Size Int -> Float
	-> RandT g Identity Labyrinth
genLabyrinth (width,height) wallRatio = 
	randomTunnels startMatrix wallRatio
		where
			startMatrix = massiveField (width,height)

-- a field with wall on all cells 
massiveField :: Size Int -> Labyrinth
massiveField (width,height) = fromJust $ mFromListRow $ replicate height line
	where
		line = replicate width Wall :: [Territory]

-- bore tunnels until the wall ratio has been reached:
randomTunnels :: (RandomGen g) => Labyrinth -> Float -> Rand g Labyrinth
randomTunnels lab wallRatio = if currentWallRatio <= wallRatio then return lab else do
	randomPos <- fromList $ zip (map swap $ mGetAllIndex lab) (repeat 1)
	let oneStepLeft = (movePoint (mGetWidth lab,mGetHeight lab) randomPos (directionToSpeed Left))
	lab' <- boreTunnel randomPos Right lab
	lab'' <- boreTunnel oneStepLeft Left lab'
	randomTunnels lab'' wallRatio -- <- recursion !
	where
		currentWallRatio = (fromIntegral countWall) / (fromIntegral $ width*height)
		countWall = F.foldl (+) 0 (fmap fromEnum lab)
		(width,height) = (mGetWidth lab, mGetHeight lab)

-- bore one tunnel:
boreTunnel ::
	RandomGen g =>
	Size Int
	-> Direction
	-> Matrix Territory
	-> RandT g Identity (Matrix Territory)
boreTunnel pos0 favDir matr = calcNewMatr (wormBehaviour (favDir,0.95)) pos0 matr WS{ lastDir=favDir }

-- |this is what a worm sees at every iteration:
--type View a st = (Matrix a, Pos)
type View st a = (Matrix a, Pos Int, st)

-- |this defines a worms behaviour:
--type Behaviour a st = (View a st -> (a,Maybe Movement,st))
type Behaviour st g a = (View st a -> Rand g (a,Maybe Movement,st))


{- allows a simple automaton ("worm") to work on a matrix
a "worm" is defined by its Behaviour...
-}
calcNewMatr ::
	Behaviour t g a
	-> Size Int
	-> Matrix a
	-> t
	-> RandT g Identity (Matrix a)
calcNewMatr beh pos0 matr st = do
	(newMatr, mov, st') <- oneStep beh pos0 matr st
	case mov of
		Nothing -> return newMatr
		Just dir -> calcNewMatr beh newPos newMatr st'
			where
				newPos = movePoint (mGetWidth matr, mGetHeight matr) pos0 (directionToSpeed dir)

-- |let a "worm" take exactly one step:
oneStep ::
	(Behaviour st g a) -> Pos Int -> Matrix a -> st
	-> Rand g ((Matrix a), Maybe Movement, st)
oneStep beh pos0 matr st = do
	(newVal,dir,st') <- beh (matr, pos0,st)
	let newMatr = mSet (swap pos0) newVal matr 
	return (newMatr,dir,st')


data WormStatus = WS {
	lastDir :: Direction
}

{- defines the behaviour of a worm. The worm will roughly go to its favourite direction,
-- until it reaches a free field.
-}
wormBehaviour ::
	--(MonadRandom m) =>
	(RandomGen g) =>
	(Direction,Rational)
	-> (Behaviour WormStatus g Territory)
wormBehaviour dirAndProp@(favDir,_) (mat, pos, WS{ lastDir=lastDir_ }) = do
	rndDir <- randomDirS (favDir:(orthogonal favDir)) [dirAndProp]
	return $ (Free,maybeDir rndDir,WS{ lastDir = rndDir })
	where
		maybeDir rndDir = if 
			(mGet (swap forwardPos) mat)/=Free &&
			(mGet (swap leftPos) mat)/=Free &&
			(mGet (swap rightPos) mat)/=Free 
			then Just $ rndDir
			else Nothing
		[forwardPos,leftPos,rightPos] = map (movePoint (mGetWidth mat,mGetHeight mat) pos . directionToSpeed) [lastDir_, leftOf lastDir_, rightOf lastDir_]

-- tests if a position is inside a given area:
{-
inBox :: Area -> Pos -> Bool
inBox (posBox,sizeBox) pos = (pos `vecGOE` posBox) && (pos `vecSOE` (posBox |+| sizeBox))
	where
		vecGOE l r = (vecX l >= vecX r) && (vecY l >= vecY r)
		vecSOE l r = (vecX l <= vecX r) && (vecY l <= vecY r)
-}
