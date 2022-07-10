module Generator.HamiltonianPath where

import Generator.Hidato(Hidato(..), replaceHidato, isValidPosition)
import Generator.Direction

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
        newSizePath = sizePath + 1
        newHidato = replaceHidato hidato row col sizePath
        matrix' = matrix hidato
        mask' = mask hidato
        freeCells' = freeCells hidato
        amountRows' = amountRows hidato
        amountCols' = amountCols hidato