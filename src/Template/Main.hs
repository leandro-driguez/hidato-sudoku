module Template.Main(addTemplate) where

import Common.Hidato 
import Common.Direction
import Template.Utils
import Generator.HamiltonianPath(searchHamiltonianPath)


addTemplate :: String -> FilePath -> IO ()
addTemplate templateName rootPath = do
    
    template <- loadTemplate templateName (rootPath ++ "/newTemplates/")

    let
        amountRows' = amountRows template - 1
        amountCols' = amountCols template - 1

        validHidato = dropWhile 
            (\(row,col) -> searchHamiltonianPath template row col Up 0 == Nil) 
            [(x,y) | x<-[0..amountRows'], y <- [0..amountCols']]

    case validHidato of
        [] -> putStrLn "\nERROR: Not valid hidato!\n"
        _ -> do
            loadTemplate <- readFile $ rootPath ++ "/newTemplates/" ++ templateName ++ ".txt"
            writeFile (rootPath ++ "/templates/" ++ templateName ++ ".txt") loadTemplate
            putStrLn ("\nOK: The template " ++ templateName ++" was added succesfully\n")
        
