module Generator.FindUniqueSolution(findUniqueSolution) where


import Common.Utils


findUniqueSolution :: (Num a, Ord a) => [[a]] -> [[Bool]] -> [[a]]
findUniqueSolution hidato mask = 
    let top = searchTop  hidato 
        pos = searchNumber 2 0 hidato
        row = fst pos
        column = snd pos
    in iteration 2 top row column hidato mask

iteration :: (Num a, Ord a) => a -> a -> a -> a -> [[a]] -> [[Bool]] -> [[a]]
iteration num top row column hidato mask | top == num = hidato
                                         | solveNumber new_hidato mask > 1 = iteration new_num top pos_row pos_column hidato mask 
                                         | otherwise = iteration new_num top pos_row pos_column new_hidato mask
                                         where new_num = num + 1
                                               new_hidato = changeState 0 row column 0 hidato
                                               pos = searchNumber new_num 0 hidato
                                               pos_row = fst pos
                                               pos_column = snd pos
                                               
solveNumber :: (Num a , Ord a ) => [[a]] -> [[Bool]] -> a
solveNumber hidato mask =
     let top = searchTop  hidato 
         pos = searchNumber 1 0 hidato
         row = fst pos
         column = snd pos
         ubicated = qsort (searchUbicated hidato)
     in  go 2 top row column ubicated hidato mask  

go :: (Num a , Ord a) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> a
go num top row column ubicated hidato mask = result
                                                  where result = giveNumber num top row column ubicated hidato mask
 
giveNumber :: (Num a , Ord a ) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> a
giveNumber num top row column (x:xs)  hidato mask | top == num && placed = 1
                                                          | top == num = 0
                                                          | alreadyUbicated num x && placed = giveNumber new_num top placed_row placed_column xs hidato mask
                                                          | alreadyUbicated num x = 0 
                                                          | otherwise = ubication_1 + ubication_2 + ubication_3 + ubication_4 + ubication_5 + ubication_6 + ubication_7 + ubication_8 
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


validPlace :: (Num a , Ord a) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> a
validPlace num top row column  ubicated hidato mask | notValidMask row column mask = 0
                                                     | notValidHidato row column hidato = 0
                                                     | otherwise = giveNumber new_num top row column ubicated new_hidato mask
                                                     where new_hidato = changeState num row column 0 hidato
                                                           new_num = num + 1
