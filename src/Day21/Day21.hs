{-# LANGUAGE RecordWildCards #-}

module Day21.Day21 where

import AdventPrelude (uniqueMapping)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Day21.Parser as Parser
import Day21.Types (Food (..), IngrMap, Ingredient)
import Types.Solution (Solution (..))

-- | Make a allergen->ingredients map from a food.
mkIngrMap :: Food -> IngrMap
mkIngrMap Food {..} = Map.fromSet (const ingredients) allergens

-- | Get the union of all ingredients from all foods.
allIngredients :: [Food] -> Set Ingredient
allIngredients = foldMap ingredients

-- | Map each allergen to all ingredients that _could_ possibly contain it.
possibilitiesMap :: [Food] -> IngrMap
possibilitiesMap = foldr (Map.unionWith Set.intersection . mkIngrMap) Map.empty

-- | Map each allergen to all ingredients that _could not_ possibly contain it.
nonPossibilitiesMap :: [Food] -> IngrMap
nonPossibilitiesMap = fmap . Set.difference <$> allIngredients <*> possibilitiesMap

-- | Get all ingredients that are guaranteed to _not_ contain an allergen.
nonPossibles :: [Food] -> Set Ingredient
nonPossibles = foldr1 Set.intersection <$> nonPossibilitiesMap

solve1 :: [Food] -> Int
solve1 foods =
  let ingrOccurrences = foldMap (MultiSet.fromSet . ingredients) foods
   in sum $ (`MultiSet.occur` ingrOccurrences) <$> Set.toList (nonPossibles foods)

solve2 :: [Food] -> Maybe Ingredient
solve2 foods = do
  mapping <- uniqueMapping . Map.toList $ possibilitiesMap foods
  return $ (List.intercalate "," . fmap snd . List.sortOn fst) mapping

part1 :: Solution
part1 =
  MkSol
    { parse = Parser.foods,
      solve = solve1
    }

part2 :: Solution
part2 =
  MkSol
    { parse = Parser.foods,
      solve = solve2
    }
