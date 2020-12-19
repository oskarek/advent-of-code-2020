module Day19.Day19 where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import qualified Day19.Parser as Parser
import Day19.Types (Rule (..))
import Types.Solution (Solution (..))

matches :: Rule -> String -> Map.Map Int Rule -> Bool
matches rule = go [rule]
  where
    go ruleStack message ruleMap = case ruleStack of
      [] -> message == ""
      (topRule : rest) -> case message of
        "" -> False
        (c : cs) -> case topRule of
          Ch ch -> c == ch && go rest cs ruleMap
          Choice subRuleIdList -> any f subRuleIdList
            where
              f subRuleIds =
                let subRules = (ruleMap Map.!) <$> subRuleIds
                 in go (subRules ++ rest) message ruleMap

countMatching0 :: (Map.Map Int Rule, [String]) -> Int
countMatching0 (ruleMap, messages) =
  let rule0 = ruleMap Map.! 0
   in length $ filter (flip (matches rule0) ruleMap) messages

replace8And11 :: Map.Map Int Rule -> Map.Map Int Rule
replace8And11 parsers =
  Map.insert 8 (Choice [[42], [42, 8]]) $ Map.insert 11 (Choice [[42, 31], [42, 11, 31]]) parsers

part1 :: Solution
part1 =
  MkSol
    { parse = Parser.input,
      solve = countMatching0
    }

part2 :: Solution
part2 =
  MkSol
    { parse = Parser.input,
      solve = countMatching0 . first replace8And11
    }
