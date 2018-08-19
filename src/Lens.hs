module Lens where

import Lens.Micro.Platform
import Language.Haskell.TH.Syntax

-- |I want lenses for field `field` named `field_l`:
lensRules' :: LensRules
lensRules' =
	lensRules & lensField .~ \_ _ field ->
				[TopName $ mkName $ nameBase field ++ "_l"]
			{-
		case nameBase field of
			'_':x:xs -> [TopName (mkName (toLower x : xs))]
			_        -> []
			-}
