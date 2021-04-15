module Score
    (
      somarPontuacao
    , adicionar
    ) where

import Data.Char

somarPontuacao :: [Int] -> Int
somarPontuacao [] = 0
somarPontuacao (a:as) | length (a:as) == 1 = a
                      | length (a:as) > 1 = a*(10^(length (a:as)-1)) + somarPontuacao as

adicionar :: Int -> [Int]
adicionar  a | a `div ` 10 == 0 = [a]
             | a `div ` 10 > 0 = adicionar (a `div ` 10) ++ [a `mod ` 10]
