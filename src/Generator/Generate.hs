module Generator.Generate(generateHidato) where

import Generator.FindUniqueSolution
import Generator.HamiltonianPath
import Common.Hidato
import Common.Direction
import Generator.Random

generateHidato :: Hidato -> IO Hidato
generateHidato hidato = do 
    randRow <- getRandom 0 (amountRows hidato - 1)
    randCol <- getRandom 0 (amountCols hidato - 1)
    let solvedHidato = searchHamiltonianPath hidato randRow randCol Up 0
    if solvedHidato == Nil
        then do generateHidato hidato
        else do 
            let matrix' = matrix solvedHidato
                mask' = mask solvedHidato
                freeCells' = freeCells solvedHidato
                amountRows' = amountRows solvedHidato
                amountCols' = amountCols solvedHidato
                newMatrix = findUniqueSolution matrix' mask'
            return Hidato{
                matrix = newMatrix,
                mask = mask',
                freeCells = freeCells',
                amountRows = amountRows',
                amountCols = amountCols'
            }