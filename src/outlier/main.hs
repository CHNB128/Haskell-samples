module Outlier where

find :: [Int] -> Int
find xs = if ((length odds) == 1) then odds !! 0 else evens !! 0
  where odd   = \e -> e `mod` 2 == 0
        even  = \e -> not $ odd e
        odds  = filter odd xs
        evens = filter even xs
