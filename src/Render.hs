{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Render where

import GameData
import Vector2D
import SGData.Matrix
import qualified Data.Foldable as F -- enables folds over matrices
import Data.Tuple
import Control.Monad.Random

import Graphics.Gloss hiding(display)
import Codec.BMP( BMP, bmpDimensions )


data ImageResources = ImageResources {
	imgRes_wallTile :: BMP
}

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

render :: MonadRandom m => ImageResources -> Vec Float -> GameState -> m Picture
render imgResources wSize =
	\case
		Playing world ->
			return $ renderGame imgResources wSize world
		Menu ->
			do
				tip <- uniform $ randomTips
				return $ renderTextArea (-wSize |/ 2) wSize menuParams $ "hspacman\npress 's' to start\nTip: " ++ tip
		GameOver statistics ->
			uniform randomTips >>= \tip ->
			return $ renderTextArea (-wSize |/ 2) wSize gameOverParams $ "GAME OVER!\nTip for the next time:\n" ++ tip
		Won statistics ->
			return $ renderTextArea (-wSize |/ 2) wSize wonParams "LEVEL ACCOMPLISHED.\nPress 's' to continue to next level"
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

renderGame imgResources wSize world =
	Pictures $
	[
		renderTextArea textPos textSize textParams $ info $ world_dbgInfo $ world
		,
		renderTextArea statusPos statusSize statusParams $ statsToText $ world
		,
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

statsToText World{ world_statistics = Statistics{..}, ..} =
	unlines . map concat $
	[ [ "level: ", show world_level ]
	, [ "points: ", show world_points, " left: ", show $ length world_dots ]
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
		textLines =
			reverse $
			join $
			map (splitLines 25) $ lines text

splitLines :: Int -> [a] -> [[a]]
splitLines len list =
	if length list <= len then [list]
	else
		let (x, xs) = splitAt len list
		in (x: splitLines len xs)

renderGameArea :: ImageResources -> World -> Picture
renderGameArea imgResources world =
	Pictures [
		renderLabyrinth (imgRes_wallTile imgResources) cellSize lab,
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

renderLabyrinth :: BMP -> Size Float -> Labyrinth -> Picture
renderLabyrinth wallTile cellSize lab =
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
				Wall ->
					Translate `uncurry` posCell $
					Scale `uncurry` sizeCell $
					Translate 0.5 0.5 $
					(Scale `uncurry` (1 |/| (vecMap fromIntegral $ bmpDimensions wallTile))) $
					bitmapOfBMP wallTile
					-- Color (greyN 0.2) $ Polygon $ rect posCell sizeCell
			where
				posCell = posFromCoords coords0
				sizeCell= posFromCoords (coords0 |+| (1,1)) |-| posCell
				posFromCoords coords = vecMap fromIntegral coords |*| cellSize

fitToArea :: Vec Float -> Vec Float -> Picture -> Picture
fitToArea pos size =
	uncurry Translate pos .
	uncurry Scale size
