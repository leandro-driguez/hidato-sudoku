module Main(main) where

import Solver.Main(solveTemplate)
import System.Directory
import Template.Main(addTemplate)
import Data.List.Split.Internals (splitOn)


main :: IO ()
main = do 
    
    rootPath <- getCurrentDirectory
    
    putStr (rootPath ++ " $ ")

    commandLine <- getLine

    let commands = splitOn " " commandLine

    case head commands of
        "solve" -> do 
                solution <- solveTemplate (commands !! 1) rootPath
                print solution
                main
        "add" -> do
                if commands !! 1 == "-t"
                        then do
                                addTemplate (commands !! 2) rootPath  
                                main
                else
                        main
        _ -> main
