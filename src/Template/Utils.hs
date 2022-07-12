module Template.Utils(loadTemplate,amountEq) where

import Common.Hidato
import Data.List.Split

loadTemplate :: String -> FilePath -> IO Hidato
loadTemplate templateName directory = do
    
    template <- readFile (directory ++ templateName ++ ".txt")
    
    let rows = splitOn "\n" template
        colums = map (splitOn " ") rows
        mask' = map (map (/= "X")) colums
        
        (freeCells', amountRows', amountCols') = amountEq mask'
        
        template' = Hidato {
            matrix = replicate amountRows' $ replicate amountCols' 0,
            mask = mask',
            freeCells = freeCells',
            amountRows = amountRows',
            amountCols = amountCols'
        }

    return template'


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


-- saveTemplate :: Hidato -> FilePath -> IO ()
