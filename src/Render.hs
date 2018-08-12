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
					concat $
					[ "hspacman\n\n"
					, "keys:\n"
					, keyInfo, "\n\n"
					, "press Space to start...\n\n\n"
					, "Tip:\n"
					, tip
					]
		GameOver _ ->
			uniform randomTips >>= \tip ->
				return $ Translate (-vecX wSize / 2) (-vecY wSize / 2) $
					renderTextArea (textAreaParams red) $
					"GAME OVER!\n\npress Space to retry the level\n\n\nTip for the next time:\n" ++ tip
		Won _ ->
			return $ Translate (-vecX wSize / 2) (-vecY wSize / 2) $
				renderTextArea (textAreaParams red) $
				"LEVEL ACCOMPLISHED.\n\nPress Space to continue to next level"
	where
		textAreaParams color = TextAreaParams {
			textArea_bmpFont = (imgRes_font imgResources),
			textArea_textParams = TextFieldParams {
				textFieldParams_size = wSize,
				textFieldParams_fontSize = 32
			},
			textArea_backgroundColor = color
		}

keyInfo = "w: up\na: left\ns: down\nd: right\nEsc: quit"

randomTips :: [String]
randomTips =
	[ "Did you notice the ghosts movements are almost random? it's so spooky...!"
	, "You can improve your strategy by not loosing!"
	, "There might be situations you can not surwive..."
	, "Stay away from the ghosts. They are dangerous!"
	, "If surwiving seems impossible, it might actually be."
	, "Sometimes succeeding is just a sideeffect of not failing"
	, "Sometimes failing is just a sideeffect of not succeeding"
	, "A wall is always just a metaphor. That you are trapped."
	, "The ghosts might not seem to have goals. But how do you know?"
	, "If you think you cannot get there, try to turn to another direction."
	, "Some people don't beleave in ghosts."
	, "Every prison looks different."
	, "A ghost appears as a cold blooded, unemotionaL being. You can learn a lot from them."
	, "The truth isn't euclidean"
	, "First of all Pacman is a game that challenges the intellect"
	]

fitToFrame :: Size Float -> Size Float -> Float
fitToFrame size frame =
	uncurry min $ (frame |/| size)

renderGame :: ImageResources -> Vec Float -> World -> Picture
renderGame imgResources wSize world =
	Translate (- vecX wSize /2) (- vecY wSize / 2) $ -- moves the origin to left bottom corner
	Pictures $
	[
		Translate `uncurry` gameAreaPos $
		Scale `uncurry` ((\x -> (x,x)) $ gameAreaScaleFactor) $
		renderGameArea imgResources world
		,
		Translate `uncurry` statusPos $
		renderTextArea (textAreaParams blue) $ statsToText world
	]
	where
		gameAreaPos = (0,0) |+| ((gameAreaSize |-| gameAreaActualSize) |/2)
		gameAreaScaleFactor = fitToFrame labyrinthSize gameAreaSize
		gameAreaActualSize = labyrinthSize |* gameAreaScaleFactor
		labyrinthSize =
			labyrinthSizeOnScreen $ world_labyrinth world
		statusPos = (0, gameAreaHeight)
		statusSize = (vecX wSize, statusHeight)
		gameAreaSize = (vecX wSize, gameAreaHeight)
		gameAreaHeight = vecY wSize - statusHeight
		statusHeight = 100
		textAreaParams color = TextAreaParams {
			textArea_bmpFont = (imgRes_font imgResources),
			textArea_textParams = TextFieldParams {
				textFieldParams_size = statusSize,
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

-- (0,0).. (labyrinthSizeOnScreen (world_labyrinth world))
renderGameArea :: ImageResources -> World -> Picture
renderGameArea imgResources world =
	Scale 1 (-1) $
	Translate 0 (- vecY labyrinthSize) $
	Pictures [
		renderLabyrinth (imgRes_floorTile imgResources) (imgRes_wallTile imgResources) (world_labyrinth world)
		,
		renderDots (world_dots world)
		,
		renderGhosts (world_ghosts world)
		,
		renderPacMan (world_t world) $ (world_pacman world)
	]
	where
		labyrinthSize = labyrinthSizeOnScreen $ world_labyrinth world

renderDots :: [Dot] -> Picture
renderDots =
	Pictures . map renderDot
	where
		renderDot dot =
			placeObject dot $
			Color black $
			ThickCircle (1/4) (1/2)

placeObject :: Object st -> Picture -> Picture
placeObject object =
	uncurry Translate (obj_pos object) .
	uncurry Scale (obj_size object)

renderPacMan :: Time -> Pacman -> Picture
renderPacMan time pacman =
	placeObject pacman $
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

renderGhosts :: [Ghost] -> Picture
renderGhosts =
	Pictures . map renderGhost

renderGhost :: Ghost -> Picture
renderGhost ghost =
	placeObject ghost $
	Color green $
	Polygon $ [(1/2,0), (1,1), (0,1) ]

-- (0,0).. (labyrinthSizeOnScreen (world_labyrinth world))
renderLabyrinth :: Picture -> Picture -> Labyrinth -> Picture
renderLabyrinth floorTile wallTile lab =
	Pictures $
		F.foldr (:) [] $ mapWithIndex `flip` lab $ \index territory ->
			Translate `uncurry` (vecMap fromI $ swap index) $
			drawCell floorTile wallTile territory

labyrinthSizeOnScreen labyrinth = swap $ vecMap fromI $ mGetSize $ labyrinth

-- (0,0) .. (1,1)
drawCell :: Picture -> Picture -> Territory -> Picture
drawCell floorTile@(Bitmap floorWidth floorHeight _ _) wallTile@(Bitmap wallWidth wallHeight _ _) =
	Translate 0.5 0.5 .
	\case
		Free ->
			Scale `uncurry` (1 |/| (vecMap fromIntegral $ (floorWidth, floorHeight))) $
			floorTile
			-- Color (greyN 0.8) $ Polygon $ rect posCell sizeCell
		Wall ->
			Scale `uncurry` (1 |/| (vecMap fromIntegral $ (wallWidth, wallHeight))) $
			wallTile
			-- Color (greyN 0.2) $ Polygon $ rect posCell sizeCell

fitToArea :: Vec Float -> Vec Float -> Picture -> Picture
fitToArea pos size =
	uncurry Translate pos .
	uncurry Scale size
