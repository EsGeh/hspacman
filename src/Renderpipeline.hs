module Renderpipeline where

import GameData
--import LevelGenerator
import SGData.Vector2D
--import Math.Matrix
import SGData.Matrix
import qualified Data.Foldable as F -- enables folds over matrices
import Data.Tuple

import Graphics.Gloss hiding(display)


type WindowSize = Vec Float

renderWorld :: WindowSize -> World -> Picture
renderWorld wSize world =
	Pictures $
	[
		fitToArea (-1/2, -1/2) (vecX wSize, -vecY wSize) $
		--fitToArea (0,textHeight) (1, 1-textHeight) $
		uncurry fitToArea gameArea $
		renderGame world,
		renderDbgText wSize textArea (world_dbgInfo world)
	]
	where
		gameArea :: (Vec Float, Vec Float)
		gameArea = ((0, textHeight), 1 |-| (0,textHeight))
		textArea :: (Vec Float, Vec Float)
		textArea = ((0,0), (1, textHeight))
		textHeight :: Float
		textHeight = 0.1

renderDbgText :: Vec Float -> (Vec Float, Vec Float) -> DebugInfo -> Picture
renderDbgText wSize textArea dbgInfo =
	Pictures $
	[
		{-
		fitToArea (-1/2, -1/2) (vecX wSize, -vecY wSize) $
		uncurry fitToArea textArea $
		Color white $
			Line $ rect (0,0) (1,1),
		-}
		--Translate 0 lineHeight $
		Pictures $
		adjustTextLines $
		map (
			Scale 0.08 0.08 .
			Color white .
			Text
		) $
			textLines
	]
	where
		adjustTextLines :: [Picture] -> [Picture]
		adjustTextLines =
			map adjust . ([0..] `zip`)
			where
				adjust :: (Int, Picture) -> Picture
				adjust (index, textLine) =
					Translate (fst textPos0) yPos textLine
					where
						yPos =
							snd textPos0 +
							lineHeight * (fromIntegral index)
		lineHeight =
			(/(fromIntegral $ length textLines)) $ (snd $ textPos1 |-| textPos0)
		textLines = lines $ info dbgInfo
		textPos0 :: Vec Float
		textPos0 =
			( (\x -> -x) $ (/2) $ fst wSize
			, (/2) $ snd wSize
			)
		textPos1 :: Vec Float
		textPos1 =
			textPos0 |-| ((snd textArea) |*| wSize)

renderGame :: World -> Picture
renderGame world =
	--Color red $ Polygon $ rect (0,0) (1,1)
	case (world_uiState world) of
		Menu ->
			renderMenu
		Playing ->
			renderGameArea world

renderMenu :: Picture
renderMenu =
	Pictures [
		Color red $ Polygon $ rect (0,0) (1,1)
	]

renderGameArea :: World -> Picture
renderGameArea world =
	Pictures [
		renderLabyrinth cellSize lab,
		renderPacMan cellSize (world_pacman world),
		renderGhosts cellSize (world_ghosts world)
	]
	where
		cellSize :: Size Float
		cellSize = (1,1) |/| (fromIntegral $ mGetWidth lab, fromIntegral $ mGetHeight lab)
		lab = world_labyrinth world 

renderPacMan :: Size Float -> Pacman -> Picture
renderPacMan cellSize pacman =
	renderChar cellSize pacman $
	Translate (1/2) (1/2) $
	Rotate rotateAngle $
	Color yellow $
	ThickArc
		(mouthAngle/2)
		(-mouthAngle/2)
		(1/4) -- radius ?
		(1/2) -- line width ?
	where
		mouthAngle = 90 * (sin $ 5 * obj_t pacman) -- [(-90)..90]
		rotateAngle =
			case vecMap signum $ obj_direction pacman of
				(1,0) -> 0
				(1,1) -> -45
				(0,1) -> -90
				(-1,1) -> -135
				(-1,0) -> -180
				(-1,-1) -> -225
				(0,-1) -> -270
				(1,-1) -> -315
				_ -> 0
				--angle -> error $ "got " ++ show angle

renderGhosts :: Size Float -> [Ghost] -> Picture
renderGhosts cellSize =
	Pictures . map (renderGhost cellSize)

renderGhost :: Size Float -> Ghost -> Picture
renderGhost cellSize ghost =
	renderChar cellSize ghost $
	Color green $
	(Polygon $ rect (0,0) (1,1))
	where

renderChar :: Size Float -> Object st -> Picture -> Picture
renderChar cellSize object =
	uncurry Translate (cellSize |*| obj_pos object) .
	uncurry Scale (cellSize |*| obj_size object)

renderLabyrinth :: Size Float -> Labyrinth -> Picture
renderLabyrinth cellSize lab =
	--Color white $ Polygon $ rect (0,0) (1,1)
	Pictures $
		F.foldr (:) [] $ mapWithIndex drawCell lab
	where
        -- in order to display the matrix correctly the lines/columns have to be flipped when drawing
		drawCell :: MatrIndex -> Territory -> Picture
		drawCell coords = drawCell' (swap coords)
		drawCell' coords0 ter =
			case ter of
				Free -> Color (greyN 0.8) $ Polygon $ rect posCell sizeCell
				Wall -> Color (greyN 0.2) $ Polygon $ rect posCell sizeCell
			where
				posCell = posFromCoords coords0
				sizeCell= posFromCoords (coords0 |+| (1,1)) |-| posCell
				posFromCoords coords = vecMap fromIntegral coords |*| cellSize

fitToArea :: Vec Float -> Vec Float -> Picture -> Picture
fitToArea (posX, posY) (width, height) =
	Scale width height .
	Translate posX posY

rect :: Pos Float -> Size Float-> [Pos Float]
rect pos (w,h) = [ pos, pos|+|(0,h), pos|+|(w,h), pos|+|(w,0), pos ]
