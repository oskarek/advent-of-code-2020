{-# LANGUAGE OverloadedStrings #-}

module Day21.Parser where

import Data.Set (Set)
import qualified Data.Set as Set
import Day21.Types (Allergen, Food (Food), Ingredient)
import Text.Megaparsec (sepBy1, sepEndBy, some)
import Text.Megaparsec.Char (letterChar, newline, space1)
import qualified Types.Parser as P

ingredients :: P.Parser (Set Ingredient)
ingredients = Set.fromList <$> some (some letterChar <* space1)

allergens :: P.Parser (Set Allergen)
allergens = Set.fromList <$> P.parens ("contains " *> (some letterChar `sepBy1` P.comma))

food :: P.Parser Food
food = Food <$> ingredients <*> allergens

foods :: P.Parser [Food]
foods = food `sepEndBy` newline
