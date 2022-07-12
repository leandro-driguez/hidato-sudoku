module Solver.Main(solveTemplate, ShowHidatos) where

import Data.List.Split
import Common.Hidato 
import Template.Utils
import Generator.Generate
import Solver.Solve


data ShowHidatos = ShowHidatos { generatedHidato :: Hidato, solvedHidato :: Hidato }

instance Show ShowHidatos where
    show (ShowHidatos g s) = "\n" ++ "Generated Hidato:" ++ show g ++ "\n" ++ "Solved Hidato:" ++ show s


solveTemplate :: String -> FilePath -> IO ShowHidatos
solveTemplate templateName rootPath = do
    
    hidato' <- loadTemplate templateName (rootPath ++ "/templates/")

    generatedHidato <- generateHidato hidato'

    let matrixGeneratedHidato = matrix generatedHidato
        maskGeneratedHidato = mask generatedHidato
        solvedHidato' = solve matrixGeneratedHidato maskGeneratedHidato 
        hidato = Hidato {
            matrix = solvedHidato',
            mask = mask hidato,
            freeCells = freeCells hidato,
            amountRows = amountRows hidato,
            amountCols = amountCols hidato
        }
        showHidatos = ShowHidatos {
            generatedHidato = generatedHidato,
            solvedHidato = hidato
        }

    return showHidatos
