module Solver.Solve (solve) where

import Common.Utils

solve :: (Num a , Ord a ) => [[a]] -> [[Bool]] -> [[a]]
solve hidato mask =
     let top = searchTop  hidato 
         pos = searchNumber 1 0 hidato
         row = fst pos
         column = snd pos
         ubicated = qsort (searchUbicated hidato)
     in  sol 2 top row column ubicated hidato mask  

sol :: (Num a , Ord a) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> [[a]]
sol num top row column ubicated hidato mask = result
                                                  where estado = ubica num top row column ubicated hidato mask
                                                        result = snd estado
 
ubica :: (Num a , Ord a ) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> (Bool, [[a]])
ubica num top row column (x:xs)  hidato mask | top == num && placed = (True, hidato)
                                             | top == num = (False,hidato)
                                             | alreadyUbicated num x && placed = ubica new_num top placed_row placed_column xs hidato mask
                                             | alreadyUbicated num x = (False , hidato) 
                                             | fst ubication_1 = ubication_1
                                             | fst ubication_2 = ubication_2
                                             | fst ubication_3 = ubication_3
                                             | fst ubication_4 = ubication_4
                                             | fst ubication_5 = ubication_5
                                             | fst ubication_6 = ubication_6
                                             | fst ubication_7 = ubication_7
                                             | fst ubication_8 = ubication_8
                                             | otherwise = (False,hidato)
                                            where new_row = 1 + row
                                                  new_column = 1 + column 
                                                  new_row_1 = row - 1 
                                                  new_column_1 = column - 1 
                                                  new_num = num + 1
                                                  placed =  correctlyPlaced num row column hidato
                                                  placed_pos = searchNumber num  0 hidato
                                                  placed_row = fst placed_pos
                                                  placed_column = snd placed_pos
                                                  ubication_1 = validPlace num top row new_column (x:xs) hidato mask 
                                                  ubication_2 = validPlace num top row new_column_1 (x:xs) hidato mask
                                                  ubication_3 = validPlace num top new_row new_column (x:xs) hidato mask
                                                  ubication_4 = validPlace num top new_row_1 new_column_1 (x:xs) hidato mask
                                                  ubication_5 = validPlace num top new_row new_column_1 (x:xs) hidato mask
                                                  ubication_6 = validPlace num top new_row_1 new_column (x:xs) hidato mask
                                                  ubication_7 = validPlace num top new_row column (x:xs) hidato mask
                                                  ubication_8 = validPlace num top new_row_1 column (x:xs) hidato mask


validPlace :: (Num a , Ord a) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> (Bool, [[a]])
validPlace num top row column  ubicated hidato mask | notValidMask row column mask = (False, hidato)
                                                     | notValidHidato row column hidato = (False, hidato)
                                                     | otherwise = ubica new_num top row column ubicated new_hidato mask
                                                     where new_hidato = changeState num row column 0 hidato
                                                           new_num = num + 1
