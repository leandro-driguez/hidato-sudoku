module Generator.Random(getRandom) where

import System.Random

getRandom :: Int -> Int -> IO Int
getRandom x y = getStdRandom $ randomR (x,y)

-- drawDouble :: Double -> Double -> IO Double
-- drawDouble x y = getStdRandom (randomR (x,y))

-- handler :: Int -> Int -> IO ()
-- handler min max = do
--     int <- getRandom min max 
--     let out = useInt int
--     writeFile ".seed" (show out) 

-- useInt :: Int -> Int
-- useInt x = x