module Utils where

import Data.List


maximumOnSafe f =
	maximumBySafe $ \x y -> f x `compare` f y

minimumOnSafe f =
	minimumBySafe $ \x y -> f x `compare` f y

maximumBySafe _ [] = Nothing
maximumBySafe f l = Just $ maximumBy f l

minimumBySafe _ [] = Nothing
minimumBySafe f l = Just $ minimumBy f l

minimumOn f = minimumBy $ \x y -> f x `compare` f y
maximumOn f = maximumBy $ \x y -> f x `compare` f y
