{-# LANGUAGE RecordWildCards #-}
module Render.Text where

import Vector2D
import Graphics.Gloss hiding(display)

import Data.Maybe
import Control.Monad


data BitmapFont = BitmapFont {
	bmpFont_bmp :: BitmapData,
	bmpFont_size :: Vec Int,
	bmpFont_charWidth :: Int
}

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
	--Translate `uncurry` (-textFieldParams_size |/ 2) $
	Scale fontScale fontScale $
	Pictures $
	zipWith positionLines
	[0..] (map (renderTextLine bmpFont textFieldParams) $ textLines)
	where
		positionLines index =
			Translate 0 (vecY textFieldParams_size - (fromIntegral $ (index+1) * lineHeight))
		lineHeight = snd $ bmpFont_size bmpFont
		textLines =
			join $
			map (splitLines (floor $ fst textFieldParams_size / (fromI charWidth)*fontScale)) $
			lines str
		charWidth = bmpFont_charWidth bmpFont
		fontScale = textFieldParams_fontSize / (fromI lineHeight)

-- | renderTextLine bmpFont textFieldParams str renders str to (0,0)..(charWidth * (length str), snd bmpFont_size)
renderTextLine :: BitmapFont -> TextFieldParams -> String -> Picture
renderTextLine bmpFont TextFieldParams{..} str =
	Pictures $
	zipWith positionChars
	[0..] (map (renderChar bmpFont) str)
	where
		positionChars :: Int -> Picture -> Picture
		positionChars index =
			Translate (fromIntegral $ index * bmpFont_charWidth bmpFont) 0
		-- (width, height) = bmpFont_size bmpFont

-- | renders a single char to (0,0)..(charWidth, snd bmpFont_size)
renderChar :: BitmapFont -> Char -> Picture
renderChar BitmapFont{..} char =
	fromMaybe (Color white $ Polygon $ rect (0,0) (1,1))$
	charToIndex char >>= \index ->
	return $
		Translate ((fromIntegral bmpFont_charWidth)/2) ((fromIntegral $ snd bmpFont_size)/2) $
		(BitmapSection `uncurry` bmpFont_size) bmpFont_bmp True
			( index * bmpFont_charWidth, 0)
			( bmpFont_charWidth, snd bmpFont_size)
	where
		charToIndex c = 
			let index = fromEnum c
			in
				if index `elem` [32..122]
				then return $ index - 32
				else Nothing

fromI = fromIntegral

splitLines :: Int -> [a] -> [[a]]
splitLines len list =
	if length list <= len then [list]
	else
		let (x, xs) = splitAt len list
		in (x: splitLines len xs)
