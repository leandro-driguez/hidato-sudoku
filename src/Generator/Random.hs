module Generator.Random(getRandom) where

import System.Random

getRandom :: Int -> Int -> IO Int
getRandom x y = getStdRandom $ randomR (x,y)
