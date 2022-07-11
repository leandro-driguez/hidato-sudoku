module Generator.HamiltonianPath
    (searchHamiltonianPath)
    where

import Common.Hidato
    ( 
        Hidato(..), 
        replaceHidato, 
        isValidPosition
    )

import Common.Direction
    (
        directionToCol,
        directionToRow,
        Direction(..) 
    )

import Generator.Random(drawInt)


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
