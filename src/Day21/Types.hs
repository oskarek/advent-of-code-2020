module Day21.Types where

import Data.Map (Map)
import Data.Set (Set)

type Allergen = String
type Ingredient = String

data Food = Food {ingredients :: Set Ingredient, allergens :: Set Allergen}
type IngrMap = Map Allergen (Set Ingredient)