module speed-calculation where

gps :: Int -> [Double] -> Int
gps s x = mSection x 0 
  where
    speed :: Double -> Double -> Double
    speed dd sec = (3600 * dd) / sec
    mSection :: [Double] -> Int -> Int
    mSection (x:y:xs) result = 
      if xs /= []
      then mSection xs (max (round (speed (y - x) s) :: Int) result) 
      else result 
      
main = 
  print $ gps s x == 74
  where 
    x = [0.0, 0.19, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25]
    s = 15.0
         
