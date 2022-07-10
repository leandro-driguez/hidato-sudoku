module Tester
    () where

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
  LowerLeftDiagonal -> -1
  Left' -> 0
  UpperLeftDiagonal -> -1


directionToCol :: Direction -> Int
directionToCol direction = case direction of
    Up -> 0
    UpperRightDiagonal -> 1
    Right' -> 1
    LowerRightDiagonal -> 1
    Down -> 0
    LowerLeftDiagonal -> 1
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
} deriving (Show, Eq)


data Tree = BinaryTree { value :: Int, left :: Tree, right :: Tree } | Leaf Int deriving (Show, Eq)


isValidPosition :: Hidato -> Int -> Int -> Bool
isValidPosition hidato row col = 
    row < amountRows' && col < amountCols'
    && row >= 0 && col >= 0 
    && matrix' !! row !! col == 0
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


searchHamiltonianPath :: Hidato -> [(Int, Int)]
searchHamiltonianPath hidato = searchHamiltonianPath' hidato 0 0 Up [] 0


searchHamiltonianPath' :: Hidato -> Int -> Int -> Direction -> [(Int, Int)] -> Int -> [(Int, Int)]
searchHamiltonianPath' hidato row col direction path sizePath
    | isValidPosition' && freeCells' == 1 = newPath 
    | isValidPosition' && newHamiltonianPath /= [] = newHamiltonianPath 
    | isValidPosition' && direction /= UpperLeftDiagonal = 
        searchHamiltonianPath' hidato row col (succ direction) path sizePath
    | otherwise = []
    where 
        newHamiltonianPath = searchHamiltonianPath' newHidato newRow newCol Up newPath newSizePath
        isValidPosition' = isValidPosition hidato row col
        newRow = row + directionToRow direction
        newCol = col + directionToCol direction
        newPath = (row, col) : path
        newMatrix = replaceMatrix matrix' row col newSizePath
        newSizePath = sizePath + 1
        newHidato = Hidato {
            matrix = newMatrix,
            mask = mask',
            freeCells = freeCells' - 1,
            amountRows = amountRows',
            amountCols = amountCols'
        }
        matrix' = matrix hidato
        mask' = mask hidato
        freeCells' = freeCells hidato
        amountRows' = amountRows hidato
        amountCols' = amountCols hidato

