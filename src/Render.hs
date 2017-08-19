{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Render(
	module Render,
	module Render.Text
) where

import Render.Text
import GameData
import Vector2D
import SGData.Matrix
import qualified Data.Foldable as F -- enables folds over matrices
import Data.Tuple
import Data.Maybe
import Control.Monad.Random

import Graphics.Gloss hiding(display)
import Codec.BMP( BMP, bmpDimensions )


data ImageResources = ImageResources {
	imgRes_wallTile :: Picture,
	imgRes_floorTile :: Picture,
	imgRes_font :: BitmapFont
}

data WindowAreas =
	WindowAreas {
		textArea :: (Vec Float, Vec Float),
		statusArea :: (Vec Float, Vec Float),
		gameArea :: (Vec Float, Vec Float)
	}

windowAreas :: WindowAreas
windowAreas = WindowAreas {
	statusArea = ((0,1-textHeight), (1,textHeight)),
	textArea = ((0,0), (1,statusHeight)),
	gameArea = ((0, statusHeight), (1,1 - textHeight - statusHeight))
}
	where
		textHeight = 0.1 :: Float
		statusHeight = 0.1 :: Float

render :: MonadRandom m => ImageResources -> Vec Float -> GameState -> m Picture
render imgResources wSize =
	\case
		Playing world ->
			return $ renderGame imgResources wSize world
		Menu ->
			do
				tip <- uniform $ randomTips
				return $ Translate (-vecX wSize / 2) (-vecY wSize / 2) $
					renderTextArea (textAreaParams green) $
					"hspacman\n\npress 's' to start\n\n\nTip:\n" ++ tip
		GameOver _ ->
			uniform randomTips >>= \tip ->
				return $ Translate (-vecX wSize / 2) (-vecY wSize / 2) $
					renderTextArea (textAreaParams red) $
					"GAME OVER!\n\n\nTip for the next time:\n" ++ tip
		Won _ ->
			return $ Translate (-vecX wSize / 2) (-vecY wSize / 2) $
				renderTextArea (textAreaParams red) $
				"LEVEL ACCOMPLISHED.\n\nPress 's' to continue to next level"
	where
		textAreaParams color = TextAreaParams {
			textArea_bmpFont = (imgRes_font imgResources),
			textArea_textParams = TextFieldParams {
				textFieldParams_size = wSize,
				textFieldParams_fontSize = 32
			},
			textArea_backgroundColor = color
		}

randomTips :: [String]
randomTips =
	[ "Did you notice the ghosts movements are almost random? it's so spooky...!"
	, "You can improve your strategy by not loosing!"
	, "There may be situations you can not surwive..."
	, "Stay away from the ghosts. They are dangerous!"
	, "If surwiving seems impossible, it might actually be."
	, "Sometimes succeeding is just a sideeffect of not failing"
	, "Sometimes failing is just a sideeffect of not succeeding"
	, "A wall is always just a metaphor. That you are trapped."
	, "The ghosts might not seem to have goals. But how do you know?"
	-- , "For you still MISTER Pacman"
	, "If you think you cannot get there, try to turn to another direction."
	, "Some people don't beleave in ghosts."
	, "Every prison looks different."
	, "A ghost appears as a cold blooded, spooky being. You can learn a lot from them."
	, "The truth isn't euclidian"
	]

renderGame :: ImageResources -> Vec Float -> World -> Picture
renderGame imgResources wSize world =
	Pictures $
	[
		{-
		Translate `uncurry` textPos $
		renderTextArea (textAreaParams white) $ info $ world_dbgInfo $ world
		,
		-}
		Translate `uncurry` statusPos $
		renderTextArea (textAreaParams blue) $ statsToText world
		,
		{-
		renderTextArea textPos textSize textParams $ info $ world_dbgInfo $ world
		,
		renderTextArea statusPos statusSize statusParams $ statsToText $ world
		,
		-}
		fitToArea (-wSize |/2)  wSize $
			fitToArea `uncurry` (gameArea windowAreas) $
			fitToArea (0,1) (1,-1) $
				renderGameArea imgResources world
	]
	where
		textPos =
			(|-| (wSize |/2)) $
			(|*| wSize) $
			(fst $ textArea windowAreas)
		textSize =
			(|*| wSize) $
			(snd $ textArea windowAreas)
		statusPos =
			(|-| (wSize |/2)) $
			(|*| wSize) $
			(fst $ statusArea windowAreas)
		statusSize =
			(|*| wSize) $
			(snd $ statusArea windowAreas)
		textAreaParams color = TextAreaParams {
			textArea_bmpFont = (imgRes_font imgResources),
			textArea_textParams = TextFieldParams {
				textFieldParams_size = textSize,
				textFieldParams_fontSize = 32
			},
			textArea_backgroundColor = color
		}

statsToText :: World -> String
statsToText World{ world_statistics = Statistics{..}, ..} =
	unlines . map concat $
	[ [ "level: ", show world_level ]
	, [ "points: ", show world_points, " left: ", show $ length world_dots ]
	]


renderGameArea :: ImageResources -> World -> Picture
renderGameArea imgResources world =
	Pictures [
		renderLabyrinth (imgRes_floorTile imgResources) (imgRes_wallTile imgResources) cellSize lab,
		renderDots cellSize (world_dots world),
		renderPacMan cellSize (world_t world) (world_pacman world),
		renderGhosts cellSize (world_ghosts world)
	]
	where
		cellSize :: Size Float
		cellSize = (1,1) |/| (fromIntegral $ mGetWidth lab, fromIntegral $ mGetHeight lab)
		lab = world_labyrinth world 

renderDots :: Size Float -> [Dot] -> Picture
renderDots cellSize dots =
	Pictures $ map renderDot dots
	where
		renderDot dot =
			renderFigure cellSize dot $
			Color black $
			ThickCircle (1/4) (1/2)

renderPacMan :: Size Float -> Time -> Pacman -> Picture
renderPacMan cellSize time pacman =
	renderFigure cellSize pacman $
	Translate (1/2) (1/2) $
	Rotate rotateAngle $
	Color yellow $
	ThickArc
		(mouthAngle/2)
		(-mouthAngle/2)
		(1/4) -- radius ?
		(1/2) -- line width ?
	where
		mouthAngle = 90 * (sin $ 5 * time) -- [(-90)..90]
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
	renderFigure cellSize ghost $
	Color green $
	Polygon $ [(1/2,0), (1,1), (0,1) ]
	-- (Polygon $ rect (0,0) (1,1))

renderFigure :: Size Float -> Object st -> Picture -> Picture
renderFigure cellSize object =
	uncurry Translate (cellSize |*| obj_pos object) .
	uncurry Scale (cellSize |*| obj_size object)

renderLabyrinth :: Picture -> Picture -> Size Float -> Labyrinth -> Picture
renderLabyrinth floorTile@(Bitmap floorWidth floorHeight _ _) wallTile@(Bitmap wallWidth wallHeight _ _) cellSize lab =
	Pictures $
		F.foldr (:) [] $ mapWithIndex drawCell lab
	where
        -- in order to display the matrix correctly the lines/columns have to be flipped when drawing
		drawCell :: MatrIndex -> Territory -> Picture
		drawCell coords = drawCell' (swap coords)
		drawCell' coords0 ter =
			case ter of
				Free ->
					Translate `uncurry` posCell $
					Scale `uncurry` sizeCell $
					Translate 0.5 0.5 $
					Scale `uncurry` (1 |/| (vecMap fromIntegral $ (floorWidth, floorHeight))) $
					floorTile
					--bitmapOfBMP floorTile
					-- Color (greyN 0.8) $ Polygon $ rect posCell sizeCell
				Wall ->
					Translate `uncurry` posCell $
					Scale `uncurry` sizeCell $
					Translate 0.5 0.5 $
					Scale `uncurry` (1 |/| (vecMap fromIntegral $ (wallWidth, wallHeight))) $
					wallTile
					--bitmapOfBMP wallTile
					-- Color (greyN 0.2) $ Polygon $ rect posCell sizeCell
			where
				posCell = posFromCoords coords0
				sizeCell= posFromCoords (coords0 |+| (1,1)) |-| posCell
				posFromCoords coords = vecMap fromIntegral coords |*| cellSize

fitToArea :: Vec Float -> Vec Float -> Picture -> Picture
fitToArea pos size =
	uncurry Translate pos .
	uncurry Scale size
