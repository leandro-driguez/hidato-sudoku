module Tester() where

import System.Random
-- import qualified Data.Vector as V
-- import qualified Data.IntSet as S
-- import qualified Data.Char as C
-- import Utils
-- import Board

data Direction = Up | UpperRightDiagonal | 
                 Right' | LowerRightDiagonal | 
                 Down | LowerLeftDiagonal | 
                 Left' | UpperLeftDiagonal  
                 deriving (Eq, Enum, Bounded, Show)


directionToRow :: Direction -> Int
directionToRow direction = case direction of
  Up -> -1
  UpperRightDiagonal -> -1
  Right' -> 0
  LowerRightDiagonal -> 1
  Down -> 1
  LowerLeftDiagonal -> 1
  Left' -> 0
  UpperLeftDiagonal -> -1


directionToCol :: Direction -> Int
directionToCol direction = case direction of
    Up -> 0
    UpperRightDiagonal -> 1
    Right' -> 1
    LowerRightDiagonal -> 1
    Down -> 0
    LowerLeftDiagonal -> -1
    Left' -> -1
    UpperLeftDiagonal -> -1




-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------


data Hidato = Hidato { 
    matrix :: [[Int]], 
    mask :: [[Bool]], 
    freeCells :: Int, 
    amountRows :: Int, 
    amountCols :: Int 
} | Nil deriving (Show, Eq)


isValidPosition :: Hidato -> Int -> Int -> Int -> Bool
isValidPosition hidato row col lastStep = 
    row < amountRows' && col < amountCols'
    && row >= 0 && col >= 0 
    && (matrix' !! row !! col == 0 || matrix' !! row !! col == lastStep + 1) 
    && mask' !! row !! col
    where matrix' = matrix hidato
          mask' = mask hidato
          amountRows' = amountRows hidato
          amountCols' = amountCols hidato   


replaceHidato :: Hidato -> Int -> Int -> Int-> Hidato
replaceHidato hidato row col value = Hidato {
        matrix = replaceMatrix (matrix hidato) row col value,
        mask = mask hidato,
        freeCells = freeCells hidato - 1,
        amountRows = amountRows hidato,
        amountCols = amountRows hidato
    } 


replace :: [a] -> Int -> a -> [a]
replace (x:xs) 0 value = value:xs
replace (x:xs) index value | index > 0 = x : replace xs (index - 1) value 


replaceMatrix :: [[a]] -> Int -> Int -> a -> [[a]]
replaceMatrix matrix row col value = 
    let newRow = replace (matrix !! row) col value 
    in replace matrix row newRow


-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------


searchHamiltonianPath :: Hidato -> IO Hidato
searchHamiltonianPath hidato = do 
    randRow <- drawInt 0 (amountRows hidato - 1)
    randCol <- drawInt 0 (amountCols hidato - 1)
    let solvedHidato = searchHamiltonianPath' hidato randRow randCol Up 0
    if solvedHidato == Nil
        then do searchHamiltonianPath hidato
        else do 
            -- get some cells
            return solvedHidato


searchHamiltonianPath' :: Hidato -> Int -> Int -> Direction -> Int -> Hidato
searchHamiltonianPath' hidato row col direction sizePath
    | isValidPosition' && freeCells' == 1 = newHidato
    | isValidPosition' && (checkingRow, checkingCol) /= (-1,-1) = 
        searchHamiltonianPath' newHidato checkingRow checkingCol Up newSizePath
    | isValidPosition' && newHamiltonianPath /= Nil = newHamiltonianPath 
    | isValidPosition' && direction /= UpperLeftDiagonal = 
        searchHamiltonianPath' hidato row col (succ direction) sizePath
    | otherwise = Nil
    where 
        newHamiltonianPath = searchHamiltonianPath' newHidato newRow newCol Up newSizePath
        isValidPosition' = isValidPosition hidato row col sizePath
        newRow = row + directionToRow direction
        newCol = col + directionToCol direction
        newSizePath = sizePath + 1
        newHidato = replaceHidato hidato row col newSizePath
        matrix' = matrix hidato
        mask' = mask hidato
        freeCells' = freeCells hidato
        amountRows' = amountRows hidato
        amountCols' = amountCols hidato
        (checkingRow, checkingCol) = checkNeighbors hidato row col newSizePath


-- check the neighbors
checkNeighbors :: Hidato -> Int -> Int -> Int -> (Int, Int)
checkNeighbors hidato row col sizePath | mask' /= [] = (newRow, newCol) 
                                       | otherwise = (-1,-1)
    where
        newRow = row + directionToRow (head mask')
        newCol = col + directionToCol (head mask') 
        matrix' = matrix hidato 
        mask' = dropWhile (not . checkNeighbors' hidato row col sizePath) 
                          [Up, UpperRightDiagonal, Right', LowerRightDiagonal, 
                           Down, LowerLeftDiagonal, Left', UpperLeftDiagonal]


checkNeighbors' :: Hidato -> Int -> Int -> Int -> Direction -> Bool
checkNeighbors' hidato row col sizePath dir = 
    isValidPosition hidato newRow newCol sizePath && 
    (sizePath + 1 == matrix' !! newRow !! newCol)
    where 
        newRow = row + directionToRow dir 
        newCol = col + directionToCol dir 
        matrix' = matrix hidato


-- seed::Int
-- seed = 40
-- generator = mkStdGen seed


-- genRandom :: Int -> Int -> StdGen -> Int
-- genRandom min max gen = fst $ randomR (min, max) gen


drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom $ randomR (x,y)

-- drawDouble :: Double -> Double -> IO Double
-- drawDouble x y = getStdRandom (randomR (x,y))

handler :: Int -> Int -> IO ()
handler min max = do
    int <- drawInt min max 
    let out = useInt int
    writeFile ".seed" (show out) 

useInt :: Int -> Int
useInt x = x

-- for :: Int -> IO ()
-- for i | i > 0 = handler 1 100 >> for (i - 1)
--       | otherwise = return ()

-- newtype Random' = Random' { seed :: Int, generator :: StdGen} deriving Show

-- genRandom' :: Int -> Int -> IO Random'
-- genRandom' min max = do
--       seed <- randomRIO (min, max)
--       return $ Random' seed (mkStdGen seed)



-- let matrixMatrix =  [[0,0,0,4,0,0],[0,0,0,5,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]]; matrixMask = [[True,True,True,True,True,False],[True,False,True,True,True,False],[True,True,False,True,True,False],[True,True,False,True,True,True],[True,False,True,True,False,True],[True,True,True,True,True,True]]; hidato = Hidato matrixMatrix matrixMask (size^2 - 8) size size; size = 6

-- let matrixMatrix =  [[0,0,0,4,0,0],
--                      [0,0,0,5,0,0],
--                      [0,0,0,0,0,0],
--                      [0,0,0,0,0,0],
--                      [0,0,0,0,0,0],
--                      [0,0,0,0,0,0]]; 
--     matrixMask = [[True,True,True,True,True,False],
--                   [True,False,True,True,True,False],
--                   [True,True,False,True,True,False],
--                   [True,True,False,True,True,True],
--                   [True,False,True,True,False,True],
--                   [True,True,True,True,True,True]];
--     hidato = Hidato matrixMatrix matrixMask (size^2 - 8) size size; 
--     size = 6

-- let matrixMatrix = [[0,2],[3,0]]; matrixMask = [[True,True],[True,True]]; hidato = Hidato matrixMatrix matrixMask (size^2) size size; size = 2
-- let matrixMatrix = [[0,2,0],[3,0,0],[4,0,0]]; matrixMask = [[True,True,True],[True,False,True],[True,True,True]]; hidato = Hidato matrixMatrix matrixMask (size^2 - 1) size size; size = 3

-- let matrixMatrix =  [[0,2,0],
--                      [3,0,0],
--                      [4,0,0]]; 
--     matrixMask = [[True,True,True],
--                   [True,False,True],
--                   [True,True,True]];
--     hidato = Hidato matrixMatrix matrixMask (size^2 - 1) size size; 
--     size = 3


-- seed::Int
-- seed = 40
-- generator = mkStdGen seed

-- selectRandomPosFrom :: [Int] -> Int
-- selectRandomPosFrom [] = -1
-- selectRandomPosFrom positions = positions !! rand where
--   n = length positions
--   (rand, _) = randomR (0,(n-1)) generator


-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------

-- parseBoard :: String -> IO String 
-- parseBoard fileName = do
--     content <- readFile fileName
--     let rows = lines content
--     let columnCount = (flip (-) $ 1) . length $ head $ rows
--     let rowsTrim = take columnCount <$> rows :: [String]

-- module Parse where



-- parseBoard :: String -> IO Board
-- parseBoard fileName = do
--     content <- readFile fileName
--     let rows = lines content
--     let columnCount = (flip (-) $ 1) . length $ head $ rows
--     let rowsTrim = take columnCount <$> rows :: [String]
--     let board = Board (V.concat $ parseLine <$> zip rowsTrim (map (columnCount *) (enumFrom 0))) columnCount
--     return board
    
-- parseLine :: (String, Int) -> V.Vector BoardTile
-- parseLine (line, lineStart) = V.fromList $ parseChar <$> zip3 line (repeat lineStart) (enumFrom 0)

-- parseChar :: (Char, Int, Int) -> BoardTile
-- parseChar ('-', lineStart, column)  = Empty $ (lineStart + column)
-- parseChar ('0', lineStart, column) = Value (lineStart + column) 0




-- parseGame :: String -> IO Board
-- parseGame fileName = do
--     content <- readFile fileName
--     let rows = lines content
--     let columnCount = length $ words $ head $ rows
--     let board = Board (V.concat $ parseLineGame <$> zip rows (map (columnCount *) (enumFrom 0))) columnCount
--     return board

-- parseLineGame :: (String, Int) -> V.Vector BoardTile
-- parseLineGame (line, lineStart) = V.fromList $ parseCell <$> zip3 (words line) (repeat lineStart) (enumFrom 0)

-- parseCell :: (String, Int, Int) -> BoardTile
-- parseCell (cell, lineStart, column)  
--     | isNumber cell = Value (lineStart + column) (read cell)
--     | otherwise  = Empty (lineStart + column)

-- isNumber :: String -> Bool
-- isNumber = (all C.isDigit) . dropWhile (\c -> c == ' ')

