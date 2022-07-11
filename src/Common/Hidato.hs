module Common.Hidato
    ( 
        Hidato(..),
        isValidPosition,
        replaceHidato
    ) where


import Common.Direction (Direction(..), directionToCol, directionToRow)


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
    