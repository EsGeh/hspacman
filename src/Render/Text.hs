{-# LANGUAGE RecordWildCards #-}
module Render.Text where

import Render.SpriteSheet
import Vector2D
import Graphics.Gloss hiding(display)

import Data.Maybe
import Control.Monad


newtype BitmapFont = BitmapFont{
	fromBitmapFont :: SpriteSheet
}
	deriving( Show )

bmpFont_bmp :: BitmapFont -> BitmapData
bmpFont_bmp = spriteSheet_bmp . fromBitmapFont
bmpFont_charSize :: BitmapFont -> (Int,Int)
bmpFont_charSize = spriteSheet_frameSize . fromBitmapFont

loadBitmapFont :: (Int, Int) -> String -> IO BitmapFont
loadBitmapFont charSize = fmap BitmapFont . loadSpriteSheet charSize

data TextFieldParams = TextFieldParams {
	textFieldParams_size :: Vec Float,
	textFieldParams_fontSize :: Float
}

data TextAreaParams = TextAreaParams {
	textArea_bmpFont :: BitmapFont,
	textArea_textParams :: TextFieldParams,
	textArea_backgroundColor :: Color
}

renderTextArea ::
	TextAreaParams
	-> String
	-> Picture
renderTextArea TextAreaParams{..} str =
	Pictures $
	[ Color textArea_backgroundColor $ Polygon $ rect (0,0) (textFieldParams_size textArea_textParams)
	, renderText textArea_bmpFont textArea_textParams $ str
	]

-- | renderText bmpFont textFieldParams str renders str to (0,0)..(textFieldParams_size textFieldParams)
-- | if there is to much text it might leave this area on the bottom
renderText ::
	BitmapFont
	-> TextFieldParams
	-> String
	-> Picture
renderText bmpFont textFieldParams@TextFieldParams{..} str =
	Scale fontScale fontScale $
	Pictures $
	zipWith positionLines
	[0..] (map (renderTextLine bmpFont textFieldParams) $ textLines)
	where
		positionLines index =
			Translate 0 (vecY textFieldParams_size - (fromIntegral $ (index+1) * charHeight))
		textLines =
			join $
			map (splitLines (floor $ fst textFieldParams_size / (fromI charWidth)*fontScale)) $
			lines str
		fontScale = textFieldParams_fontSize / (fromI charHeight)
		(charWidth, charHeight) = bmpFont_charSize bmpFont

-- | renderTextLine bmpFont textFieldParams str renders str to (0,0)..(charWidth * (length str), snd bmpFont_size)
renderTextLine :: BitmapFont -> TextFieldParams -> String -> Picture
renderTextLine bmpFont TextFieldParams{..} str =
	Pictures $
	zipWith positionChars
	[0..] (map (renderChar bmpFont) str)
	where
		positionChars :: Int -> Picture -> Picture
		positionChars index =
			Translate
				(fromIntegral $ index * (fst $ bmpFont_charSize bmpFont))
				0

-- | renders a single char to (0,0)..(charWidth, snd bmpFont_size)
renderChar :: BitmapFont -> Char -> Picture
renderChar BitmapFont{ fromBitmapFont=spriteSheet } char =
	fromMaybe (Color white $ Polygon $ rect (0,0) (1,1)) $
	fmap (
		Translate
			(fromIntegral frameWidth / 2)
			(fromIntegral frameHeight / 2)
		.
		renderSpriteSheet spriteSheet
	) frame
	where
		(frameWidth, frameHeight) = spriteSheet_frameSize spriteSheet
		frame =
			let index = fromEnum char
			in
				if index `elem` [32..122]
				then return $ index - 32
				else Nothing


fromI :: Int -> Float
fromI = fromIntegral

splitLines :: Int -> [a] -> [[a]]
splitLines len list =
	if length list <= len then [list]
	else
		let (x, xs) = splitAt len list
		in (x: splitLines len xs)
