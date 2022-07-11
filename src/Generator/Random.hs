module Generator.Random(drawInt) where

import System.Random

drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom $ randomR (x,y)

-- drawDouble :: Double -> Double -> IO Double
-- drawDouble x y = getStdRandom (randomR (x,y))

-- handler :: Int -> Int -> IO ()
-- handler min max = do
--     int <- drawInt min max 
--     let out = useInt int
--     writeFile ".seed" (show out) 

-- useInt :: Int -> Int
-- useInt x = x