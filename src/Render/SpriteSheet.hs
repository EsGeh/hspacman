{-# LANGUAGE RecordWildCards #-}
module Render.SpriteSheet where

import Graphics.Gloss hiding(display)

import qualified Codec.BMP as BMP


data SpriteSheet = SpriteSheet {
	spriteSheet_bmp :: BitmapData,
	spriteSheet_frameSize :: (Int,Int)
}
	deriving( Show )


loadSpriteSheet :: (Int, Int) -> String -> IO SpriteSheet
loadSpriteSheet spriteSheet_frameSize filename =
	do
		bmp <-
			fmap (either (error . ("bmp error: "++) . show) id ) $ BMP.readBMP $ filename
		-- putStrLn $ ("font info: " ++) . show $ BMP.bmpFileHeader bmp
		-- putStrLn $ ("font info: " ++) . show $ BMP.bmpBitmapInfo bmp
		let spriteSheet_bmp = bitmapDataOfBMP bmp
		return $ SpriteSheet {
			..
			}

-- | renders a single char to (0,0)..(spriteSheet_frameWidth, snd bmpFont_size)
renderSpriteSheet :: SpriteSheet -> Int -> Picture
renderSpriteSheet spriteSheet@SpriteSheet{..} frame =
	--fromMaybe (Color white $ Polygon $ rect (0,0) (1,1))$
	BitmapSection 
		(indexToSubsection spriteSheet frame)
		spriteSheet_bmp
	where


indexToSubsection :: SpriteSheet -> Int -> Rectangle
indexToSubsection SpriteSheet{..} frame =
	Rectangle {
		rectPos = (col* frameWidth, row * frameHeight),
		rectSize = (frameWidth, frameHeight)
	}
	where
		col = frame `mod` colCount
		row = (frame `div` colCount) `mod` rowCount
		colCount = bmpWidth `div` frameWidth
		rowCount = bmpHeight `div` frameHeight
		(frameWidth, frameHeight) = spriteSheet_frameSize
		(bmpWidth, bmpHeight) = bitmapSize spriteSheet_bmp
