module Renderpipeline where

import GameData
--import LevelGenerator
import SGData.Vector2D
--import Math.Matrix
import SGData.Matrix
import qualified Data.Foldable as F -- enables folds over matrices
import Data.Tuple

import Graphics.Gloss hiding(display)

-- used to represent coordinates relative to an area on the screen:
-- (0,0)..(1,1)
type NormalCoords = Vec Float
type NormalSize = Vec Float


-- origin is the upper left corner of the screen (x-Axis right, y-Axis dowdown
type PosOnScreen = Vec Float
--  (0,0) .. (screenWidth,screenHeight)	
-- x right, y down

type SizeOnScreen = Vec Float
type AreaOnScreen = (PosOnScreen,SizeOnScreen)

-- nice aliases:
type WindowSize = SizeOnScreen
type DestAreaOnScreen = AreaOnScreen


-- origin is the center of the screen!, (x-Axis right, y-Axis up)
type GlossCoords = Vec Float


renderWorld :: WindowSize -> World -> Picture
renderWorld wSize world =
	case (world_uiState world) of
		Menu ->
			renderMenu wSize menuArea world
			where menuArea = ((0,0),wSize)
		Playing ->
			renderGame wSize textArea gameArea world
			where
				gameArea = ((0, textHeight), wSize |-| (0,textHeight))
				textArea = ((0,0), (vecX wSize, textHeight))
				textHeight = 50
				{-
				gameArea = ((textAreaWidth,0), wSize |-| (textAreaWidth,0))
				textArea =
					((0,0), (textAreaWidth,vecY wSize))
				textAreaWidth = 50
				-}

renderMenu :: WindowSize -> DestAreaOnScreen -> World -> Picture
renderMenu wSize destArea _ = Color yellow $ Polygon $
	map (normalizedPosToGloss wSize destArea) $ rect (0,0) (1,1)

renderGame :: WindowSize -> DestAreaOnScreen -> DestAreaOnScreen -> World -> Picture
renderGame wSize dbgTextArea gameArea world =
	Pictures $ [
		renderGameArea wSize gameArea world ,
		renderDbgText wSize dbgTextArea world
	]

renderGameArea :: WindowSize -> DestAreaOnScreen -> World -> Picture
renderGameArea wSize destArea world =
	Pictures [
		Color white $ Line (fmap (normalizedPosToGloss wSize destArea) $ rect (0,0) (1,1)),
		renderLabyrinth wSize destArea cellSize (world_labyrinth world),
		renderPacMan wSize destArea cellSize (world_pacman world)
		--renderGhosts wSize destArea cellSize (world_ghosts world)
	]
	where
		cellSize :: Size Float
		cellSize = (1,1) |/| (fromIntegral $ mGetWidth lab, fromIntegral $ mGetHeight lab)
		lab = world_labyrinth world 


renderPacMan :: WindowSize -> DestAreaOnScreen -> Size Float -> Pacman -> Picture
renderPacMan wSize gameArea cellSize pacman =
	renderChar wSize gameArea cellSize pacman $
	Translate (1/2) (-1/2) $
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
				(1,1) -> 45
				(0,1) -> 90
				(-1,1) -> 135
				(-1,0) -> 180
				(-1,-1) -> 225
				(0,-1) -> 270
				(1,-1) -> 315
				_ -> 0
				--angle -> error $ "got " ++ show angle

renderGhosts :: WindowSize -> DestAreaOnScreen -> Size Float -> [Ghost] -> Picture
renderGhosts wSize gameArea cellSize =
	Pictures . map (renderGhost wSize gameArea cellSize)


renderGhost :: WindowSize -> DestAreaOnScreen -> Size Float -> Ghost -> Picture
renderGhost wSize gameArea cellSize ghost = renderChar wSize gameArea cellSize ghost$
	Translate (0) (-1) $
	--Translate (1/2) (-1/2) $
	Color green $
	(Polygon $ rect (0,0) (1,1))
	where
		--mouthAngle = 90 * (sin $ 5 * obj_t ghost)


renderChar :: WindowSize -> DestAreaOnScreen -> Size Float -> Object st -> Picture -> Picture
renderChar wSize gameArea cellSize obj pic =
	(uncurry Translate) (normalizedPosToGloss wSize gameArea (cellSize |*| (obj_pos obj))) $
	(uncurry Scale) (obj_size obj) $
	(uncurry Scale) (normalizedPosToScreen gameArea cellSize |-| normalizedPosToScreen gameArea (0,0)) $
	pic

renderLabyrinth :: WindowSize -> DestAreaOnScreen -> Size Float -> Labyrinth -> Picture
renderLabyrinth wSize destArea cellSize lab = Pictures $ F.foldr (:)[] $ mapWithIndex drawCell lab
	where
        -- in order to display the matrix correctly the lines/columns have to be flipped when drawing
		drawCell :: MatrIndex -> Territory -> Picture
		drawCell coords = drawCell' (swap coords)
		drawCell' coords0 ter = case ter of
			Free -> Color (greyN 0.8) $ Polygon $ rect posCell sizeCell
			Wall -> Color (greyN 0.2) $ Polygon $ rect posCell sizeCell
	    		where
				posCell = posFromCoords coords0
				sizeCell= posFromCoords (coords0 |+| (1,1)) |-| posCell
				posFromCoords coords = normalizedPosToGloss wSize destArea $ vecMap fromIntegral coords |*| cellSize

rect :: NormalCoords -> NormalSize -> [NormalCoords]
rect pos (w,h) = [ pos, pos|+|(0,h), pos|+|(w,h), pos|+|(w,0), pos ]

renderDbgText :: WindowSize -> DestAreaOnScreen -> World -> Picture
renderDbgText wSize destArea world =
	Pictures [
		renderLines 0 $ lines $ info $ world_dbgInfo world,
		Color white $ Line (fmap (normalizedPosToGloss wSize destArea) $ rect (0,0) (1,1))
	]
	where
		renderLines :: Float -> [String] -> Picture
		renderLines yPos lines' = case lines' of
			[] -> Blank
			(x:xs) -> Pictures $ [
				(uncurry Translate) (normalizedPosToGloss wSize destArea (0,0.1+yPos)) $
				Color white $ 
				Scale 0.1 0.1 $
				Text x,
				renderLines (yPos+lineHeight) xs ]
			where lineHeight = min 
				((1-yPos) / (fromIntegral $ length lines'))
				0.1

-- coordinate translaters:

normalizedPosToGloss :: WindowSize -> DestAreaOnScreen -> NormalCoords -> GlossCoords
normalizedPosToGloss wSize destArea pos = toGloss wSize $ normalizedPosToScreen destArea pos

normalizedPosToScreen :: DestAreaOnScreen -> NormalCoords -> PosOnScreen
normalizedPosToScreen (posOnScr,sizeOnScr) pos = posOnScr |+| pos |*| sizeOnScr

{- game programmers are used to (0,0) to be in the left upper corner of the screen,
-- and (ScreenWidth,ScreenHeight) to be the right bottom corner.
-- GLOSS uses ccoords whose origin is the center of the screen,
-- x-Axis and y-Axis pointint right/up (like in math)
-}
toGloss :: WindowSize -> PosOnScreen -> GlossCoords
toGloss wSize pos = (pos |-| (wSize |/ 2)) |*| (1,-1)

fromGloss :: WindowSize -> GlossCoords -> PosOnScreen
fromGloss wSize pos = pos |*| (1,-1) |+| (wSize |/ 2)
