module Solver.Main(solveTemplate) where

import Data.List.Split
import Common.Hidato 
import Generator.Generate
import Solver.Solve

rootPath :: String
rootPath = "/home/leandro/Study/3rd_Year/1st_semester/Declarative_Programming/Haskell/hidato-sudoku"


data ShowHidatos = ShowHidatos { generatedHidato :: Hidato, solvedHidato :: Hidato }

instance Show ShowHidatos where
    show (ShowHidatos g s) = "\n" ++ "Generated Hidato:" ++ show g ++ "\n" ++ "Solved Hidato:" ++ show s

solveTemplate :: String -> IO ShowHidatos
solveTemplate templateName = do
    template <- readFile (rootPath ++ "/templates/" ++ templateName ++ ".txt")
    let rows = splitOn "\n" template
        colums = map (splitOn " ") rows
        mask' = map (map (/= "X")) colums
        (freeCells', amountRows', amountCols') = amountEq mask'
        hidato' = Hidato {
            matrix = replicate amountRows' $ replicate amountCols' 0,
            mask = mask',
            freeCells = freeCells',
            amountRows = amountRows',
            amountCols = amountCols'
        }
    generatedHidato <- generateHidato hidato'

    let matrixGeneratedHidato = matrix generatedHidato
        maskGeneratedHidato = mask generatedHidato
        solvedHidato' = solve matrixGeneratedHidato maskGeneratedHidato 
        hidato = Hidato {
            matrix = solvedHidato',
            mask = mask',
            freeCells = freeCells',
            amountRows = amountRows',
            amountCols = amountCols'
        }
        showHidatos = ShowHidatos {
            generatedHidato = generatedHidato,
            solvedHidato = hidato
        }

    return showHidatos


amountEq :: [[Bool]] -> (Int, Int, Int)
amountEq = foldr (
        \row (freeCells, rows, cols) -> 
        let (freeCells', amountCols) = foldr (
                \cell (freeCells'',cols) -> 
                    if cell then (freeCells''+1,cols+1) else (freeCells'',cols+1)
                ) (0,0) row 
        in 
            if amountCols == cols && rows /= 0
                then (freeCells + freeCells', rows + 1, cols)
            else if rows == 0
                then (freeCells', 1, amountCols)
            else
                (-1,-1,-1)
    ) (0,0,0)

