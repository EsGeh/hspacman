{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module LevelGenerator(
	WorldParams(..),
	genWorld,
	genLabyrinth
) where

import GameData --hiding(Direction)
import Vector2D
import Prelude hiding(Left,Right)
import Data.Tuple( swap )

import SGData.Matrix
import Control.Monad.Random
import Data.Maybe( fromMaybe )
import Data.List

import Lens.Micro.Platform

-- import Debug.Trace


-- |arguments for the level generator
data WorldParams = WorldParams {
	worldParams_level :: Int,
	worldParams_size :: Size Int,
	worldParams_gridStep :: Size Int,
	--worldParams_wallRatio :: Float,
	worldParams_ghostCount :: Int,
	worldParams_pacmanSpeed :: Float,
	worldParams_ghostsSpeed :: Float
}
	deriving( Show, Read, Ord, Eq)


genWorld :: MonadRandom m => WorldParams -> m World
genWorld WorldParams{..} =
	do
		labyrinth <- genLabyrinth worldParams_size worldParams_gridStep
		-- traceM $ "labyrinth:\n" ++ show labyrinth
		let allFreePositions =
			filter ((==Free) . (labyrinth_get `flip` labyrinth)) $
			labyrinth_allPositions labyrinth
		pacmanPos <-
			uniform $
			allFreePositions
		monsterPositions <-
			randomSubSet worldParams_ghostCount $
			filter ((>=3) . (distance $ vecMap fromIntegral $ pacmanPos) . vecMap fromIntegral) $
			allFreePositions
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
				world_dbgInfo = [],
				world_userInput = [],
				world_t = 0
				--world_randomGen = rndGen
			}
	where
		dotSize = (0.2, 0.2)

-- | `randomSubSet count list` == subset of `l` with `count` elements
randomSubSet :: (MonadRandom m, Eq a) => Int -> [a] -> m [a]
randomSubSet count list
	| count == 0 = return []
	| otherwise =
		do
			x <- uniform list
			fmap (x :) $ randomSubSet (count-1) (list\\[x])

genLabyrinth ::
	forall m .
	MonadRandom m =>
	Size Int -> Size Int
	-> m Labyrinth
genLabyrinth labSize gridStep = 
	do
		let positions =
			(|+| vecMap (`div` 2) gridStep) <$>
			mkGrid gridStep (labSize |-| gridStep)
				:: Grid (Pos Int)
		-- traceM $ "grid:\n" ++ show positions
		connections <-
			randomDelEdgesWhile edgeDelCondition $
			allEdgesOfGrid labSize positions
			:: m [Edge]
		-- traceM $ "connections:\n" ++ show connections
		let allPaths =
			map (map (pointInSize labSize) . uncurry connectionRoute) connections :: [[Pos Int]]
		-- traceM $ "allPaths:\n" ++ show allPaths
		return $
			foldl (.) id (map `flip` join allPaths $ \pos -> labyrinth_set pos Free) $
			massiveField labSize
	where
		edgeDelCondition :: [Edge] -> Edge -> Bool
		edgeDelCondition allEdges (p1, p2) =
			length (edgesFromPos labSize p1 allEdges) > 3 && length (edgesFromPos labSize p2 allEdges) > 3

type Edge = (Pos Int, Pos Int)

randomDelEdgesWhile :: MonadRandom m => ([Edge] -> Edge -> Bool) -> [Edge] -> m [Edge]
randomDelEdgesWhile cond edges =
	do
		mNewGraph <- randomDeleteEdge cond edges
		case mNewGraph of
			Nothing -> return edges
			Just x -> randomDelEdgesWhile cond x

randomDeleteEdge :: MonadRandom m => ([Edge] -> Edge -> Bool) -> [Edge] -> m (Maybe [Edge])
randomDeleteEdge cond edges =
	let possibleEdges = filter (cond edges) edges
	in 
		case possibleEdges of
			[] -> return Nothing
			_ ->
				do
					edgeToDel <- uniform possibleEdges
					return $ Just $ edges \\ [edgeToDel]

edgesFromPos :: Size Int -> Pos Int -> [Edge] -> [Edge]
edgesFromPos size =
	(\pos -> filter $ \edge -> pointInSize size (fst edge) == pos || pointInSize size (snd edge) == pos)
	.
	pointInSize size

-- an edge over the border of the torus is represented by one node in
-- (|+|) <$> [(vecX size,0), (0,vecY size), size] <*> [0..(size-1)]
allEdgesOfGrid :: Size Int -> Grid (Pos Int) -> [(Pos Int, Pos Int)]
allEdgesOfGrid size grid =
	-- nub $
	concat $
	mapWithIndex edgesToNeighbours grid
	where
		edgesToNeighbours index pos =
			do
			neighbour <- lookupIndex <$> neighbourIndices' index :: [Pos Int]
			return $ 
				if pos < neighbour then (pos, neighbour) else (neighbour, pos)
		lookupIndex index =
			let
				normalizedIndex = pointInSize (mGetSize grid) index
			in
				(if snd normalizedIndex < snd index then (|+| (vecX size,0)) else id) $
				(if fst normalizedIndex < fst index then (|+| (0,vecY size)) else id) $
				mGet normalizedIndex grid
		neighbourIndices' point =
			filter (\x -> vecX x >= 0 && vecY x >= 0) $
			neighbourIndices point

neighbourIndices :: Pos Int -> [Pos Int]
neighbourIndices point =
	do
		movement <- [(0,1), (1,0) ]
		return $
			point |+| movement

type Grid a = Matrix a

instance (Show a) => Show (Matrix a) where
	show = showMatr

mkGrid :: Vec Int  -> Size Int -> Grid (Vec Int)
mkGrid (stepX, stepY) size =
	fromMaybe (error "internal error") $ mFromListRow rows
	where
		rows :: [[Vec Int]]
		rows =
			do
				y <- [0,stepY..vecY size-1]
				return $ do
					x <- [0,stepX..vecX size-1]
					return (x,y)

-- a field with wall on all cells 
massiveField :: Size Int -> Labyrinth
massiveField (width,height) =
	fromMaybe (error "internal error") $ labyrinth_fromRows $ replicate height line
	where
		line = replicate width Wall :: [Terrain]

connectionRoute :: Vec Int -> Vec Int -> [Vec Int]
connectionRoute l r =
	lineRaster start stop
	`union`
	if (abs $ lineGradient start stop) <= 1
		then init $ lineRaster (start |+| (1,0)) (stop |+| (1,0))
		else init $ lineRaster (start |+| (0,1)) (stop |+| (0,1))
	where
		(start, stop) = (vecMap fromI l, vecMap fromI r)

lineRaster :: Vec Float -> Vec Float -> [Vec Int]
lineRaster start stop =
	(if swapCoords then map swap else id) $
		lineRaster' l r
	where
		swapCoords = not $ abs (lineGradient start stop) <= 1
		(l, r) =
			(\(l', r') -> if vecX l' > vecX r' then (r',l') else (l',r')) $
			(if swapCoords then \(l', r') -> (swap l', swap r') else id) $
			(start, stop)

-- | precondition: abs lineGradient <= 1
lineRaster' :: Vec Float -> Vec Float -> [Vec Int]
lineRaster' start stop =
	--trace ("start, stop: " ++ show (start,stop)) $
	let
		(rowDiff, colDiff) =
				stop |-| start
			:: Vec Float
		rowVals =
			[(vecX start)..(vecX stop)]
 			:: [Float]
	in 
		--trace ("rowVals " ++ show rowVals) $
		--trace ("diff " ++ show (rowDiff, colDiff)) $
		map `flip` rowVals $ \row ->
			-- trace ("row " ++ show row) $
			let
				colLeft = (row - vecX start) * colDiff / rowDiff + (vecY start) :: Float
			in
				(floor row, floor colLeft)

lineGradient :: Vec Float -> Vec Float -> Float
lineGradient start stop =
	let
		(xDiff, yDiff) =
				stop |-| start
			:: Vec Float
	in
		yDiff / xDiff

-- |abbreviation:
fromI :: (Integral i, Num a) => i -> a
fromI = fromIntegral
