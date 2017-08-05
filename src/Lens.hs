module Lens where

import Data.Char
import Lens.Micro.Platform
import Language.Haskell.TH.Syntax

lensRules' =
	lensRules & lensField .~ \_ _ field ->
				[TopName $ mkName $ nameBase field ++ "_l"]
			{-
		case nameBase field of
			'_':x:xs -> [TopName (mkName (toLower x : xs))]
			_        -> []
			-}
