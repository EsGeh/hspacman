{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Render where

import GameData
import Vector2D
import SGData.Matrix
import qualified Data.Foldable as F -- enables folds over matrices
import Data.Tuple

import Graphics.Gloss hiding(display)


data WindowAreas =
	WindowAreas {
		textArea :: (Vec Float, Vec Float),
		statusArea :: (Vec Float, Vec Float),
		gameArea :: (Vec Float, Vec Float)
	}
windowAreas = WindowAreas {
	textArea = ((0,1-textHeight), (1,textHeight)),
	statusArea = ((0,0), (1,statusHeight)),
	gameArea = ((0, statusHeight), (1,1 - textHeight - statusHeight))
}
	where
		textHeight = 0.1 :: Float
		statusHeight = 0.1 :: Float

render :: Vec Float -> GameState -> Picture
render wSize = \case
	Playing world ->
		renderGame wSize world
	Menu -> renderTextArea (-wSize |/ 2) wSize menuParams "hspacman\npress 's' to start"
	GameOver statistics -> renderTextArea (-wSize |/ 2) wSize gameOverParams "GAME OVER!"
	Won statistics -> renderTextArea (-wSize |/ 2) wSize wonParams "LEVEL ACCOMPLISHED.\nPress 's' to continue to next level"
	where
		menuParams = TextAreaParams {
			textArea_fontSize = 0.4,
			textArea_fontColor = red,
			textArea_areaColor = green
		}
		gameOverParams = TextAreaParams {
			textArea_fontSize = 0.4,
			textArea_fontColor = green,
			textArea_areaColor = red
		}
		wonParams = TextAreaParams {
			textArea_fontSize = 0.4,
			textArea_fontColor = green,
			textArea_areaColor = blue
		}

renderGame wSize world =
	Pictures $
	[
	renderTextArea textPos textSize textParams $ info $ world_dbgInfo $ world
	, renderTextArea statusPos statusSize statusParams $ statsToText $ world_statistics world
	, fitToArea (-wSize |/2)  wSize $
		fitToArea `uncurry` (gameArea windowAreas) $
		fitToArea (0,1) (1,-1) $
			renderGameArea world
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
		textParams = TextAreaParams {
			textArea_fontSize = 0.2,
			textArea_fontColor = white,
			textArea_areaColor = black
		}
		statusParams = TextAreaParams {
			textArea_fontSize = 0.2,
			textArea_fontColor = blue,
			textArea_areaColor = red
		}

statsToText Statistics{..} =
	unlines . map concat $
	[ [ "level: ", show world_level ]
	, [ "points: ", show world_points ]
	]

data TextAreaParams = TextAreaParams {
	textArea_fontSize :: Float,
	textArea_fontColor :: Color,
	textArea_areaColor :: Color
}

renderTextArea :: Vec Float -> Vec Float -> TextAreaParams -> String -> Picture
renderTextArea pos size TextAreaParams{..} text =
	Pictures $
	[ fitToArea pos size $ Color textArea_areaColor $ Polygon $ rect (0,0) (1,1)
	, Pictures $
		map `flip` (textPositions `zip` textLines) $ \(textPos, textLine) ->
			Translate `uncurry` (pos |+| (leftBorder* vecX size,textPos)) $ Scale textArea_fontSize textArea_fontSize $ Color textArea_fontColor $ Text textLine
	]
	where
		leftBorder = 0.1
		textPositions = map ((*lineHeight)) [1..]
		lineHeight = vecY size / (fromIntegral $ length textLines + 1)
		textLines = reverse $ lines text

renderGameArea :: World -> Picture
renderGameArea world =
	Pictures [
		renderLabyrinth cellSize lab,
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
			renderChar cellSize dot $
			Color black $
			ThickCircle (1/4) (1/2)

renderPacMan :: Size Float -> Time -> Pacman -> Picture
renderPacMan cellSize time pacman =
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
	renderChar cellSize ghost $
	Color green $
	Polygon $ [(1/2,0), (1,1), (0,1) ]
	-- (Polygon $ rect (0,0) (1,1))

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
fitToArea pos size =
	uncurry Translate pos .
	uncurry Scale size