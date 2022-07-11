module Generator.Generate where

find_unique_solution :: (Num a, Ord a) => [[a]] -> [[Bool]] -> [[a]]
find_unique_solution hidato mask = 
    let top = search_top  hidato 
        pos = search_number 2 0 hidato
        row = fst pos
        column = snd pos
    in iteration 2 top row column hidato mask

iteration :: (Num a, Ord a) => a -> a -> a -> a -> [[a]] -> [[Bool]] -> [[a]]
iteration num top row column hidato mask | top == num = hidato
                                         | solve_number new_hidato mask > 1 = iteration new_num top pos_row pos_column hidato mask 
                                         | otherwise = iteration new_num top pos_row pos_column new_hidato mask
                                         where new_num = num + 1
                                               new_hidato = change_state 0 row column 0 hidato
                                               pos = search_number new_num 0 hidato
                                               pos_row = fst pos
                                               pos_column = snd pos
                                               
solve_number :: (Num a , Ord a ) => [[a]] -> [[Bool]] -> a
solve_number hidato mask =
     let top = search_top  hidato 
         pos = search_number 1 0 hidato
         row = fst pos
         column = snd pos
         ubicated = qsort (search_ubicated hidato)
     in  go 2 top row column ubicated hidato mask  

go :: (Num a , Ord a) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> a
go num top row column ubicated hidato mask = result
                                                  where result = give_me_the_number num top row column ubicated hidato mask
 
give_me_the_number :: (Num a , Ord a ) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> a
give_me_the_number num top row column (x:xs)  hidato mask | top == num && placed = 1
                                                          | top == num = 0
                                                          | already_ubicated num x && placed = give_me_the_number new_num top placed_row placed_column xs hidato mask
                                                          | already_ubicated num x = 0 
                                                          | otherwise = ubication_1 + ubication_2 + ubication_3 + ubication_4 + ubication_5 + ubication_6 + ubication_7 + ubication_8 
                                                          where new_row = 1 + row
                                                                new_column = 1 + column 
                                                                new_row_1 = row - 1 
                                                                new_column_1 = column - 1 
                                                                new_num = num + 1
                                                                placed =  correctly_placed num row column hidato
                                                                placed_pos = search_number num  0 hidato
                                                                placed_row = fst placed_pos
                                                                placed_column = snd placed_pos
                                                                ubication_1 = valid_place num top row new_column (x:xs) hidato mask 
                                                                ubication_2 = valid_place num top row new_column_1 (x:xs) hidato mask
                                                                ubication_3 = valid_place num top new_row new_column (x:xs) hidato mask
                                                                ubication_4 = valid_place num top new_row_1 new_column_1 (x:xs) hidato mask
                                                                ubication_5 = valid_place num top new_row new_column_1 (x:xs) hidato mask
                                                                ubication_6 = valid_place num top new_row_1 new_column (x:xs) hidato mask
                                                                ubication_7 = valid_place num top new_row column (x:xs) hidato mask
                                                                ubication_8 = valid_place num top new_row_1 column (x:xs) hidato mask


already_ubicated :: (Num a,Ord a) => a -> a -> Bool
already_ubicated num x | x == num = True
                       | otherwise = False

correctly_placed :: (Num a, Ord a ) => a -> a -> a -> [[a]] -> Bool
correctly_placed num row column hidato | row + 1 == row_pos && column + 1 == column_pos = True
                                       | row - 1 == row_pos && column + 1 == column_pos = True
                                       | row == row_pos && column + 1 == column_pos = True
                                       | row == row_pos && column - 1 == column_pos = True
                                       | row - 1 == row_pos && column - 1 == column_pos = True
                                       | row + 1 == row_pos && column - 1 == column_pos = True
                                       | row - 1 == row_pos && column == column_pos = True
                                       | row + 1 == row_pos && column == column_pos = True
                                       | otherwise = False
                                       where pos = search_number num 0 hidato
                                             row_pos = fst pos
                                             column_pos = snd pos

valid_place :: (Num a , Ord a) => a -> a -> a -> a -> [a] -> [[a]] -> [[Bool]] -> a
valid_place num top row column  ubicated hidato mask | not_valid_in_mask row column mask = 0
                                                     | not_valid_in_hidato row column hidato = 0
                                                     | otherwise = give_me_the_number new_num top row column ubicated new_hidato mask
                                                     where new_hidato = change_state num row column 0 hidato
                                                           new_num = num + 1

change_state :: (Num a,Eq a) => a -> a -> a -> a -> [[a]] -> [[a]]
change_state num row column aux_r (x:xs) | row == aux_r = change_this_row num column 0 x : xs
                                         | otherwise = x : change_state num row column new_row xs
                                         where new_row = aux_r + 1  

change_this_row :: (Num a,Eq a) => a-> a -> a -> [a] -> [a]
change_this_row num column aux_c (x:xs) | column == aux_c =  num : xs
                                        | otherwise = x : change_this_row num column new_column xs
                                         where new_column = aux_c + 1 

not_valid_in_hidato :: (Num a, Eq a) => a -> a -> [[a]] -> Bool
not_valid_in_hidato row column hidato | check_hidato row column 0 hidato = False
                                  | otherwise = True

not_valid_in_mask :: (Num a, Eq a) => a -> a -> [[Bool]] -> Bool
not_valid_in_mask row column mask | get_rows mask == row = True
                                  | row == -1 = True
                                  | column == -1 = True
                                  | column == get_columns mask = True
                                  | check_mask row column 0 mask = True
                                  | otherwise = False

check_hidato :: (Eq a,Num a) => a -> a -> a -> [[a]] -> Bool
check_hidato row column aux_r (x:xs) | row == aux_r = distint_zero column 0 x
                                     | otherwise = check_hidato row column new_row xs 
                                     where new_row = aux_r + 1

check_mask :: (Eq a,Num a) => a -> a -> a -> [[Bool]] -> Bool
check_mask row column aux_r (x:xs) | row == aux_r = check_column column 0 x
                                   | otherwise = check_mask row column new_row xs 
                                   where new_row = aux_r + 1

distint_zero :: (Eq a, Num a) => a -> a -> [a] -> Bool
distint_zero column aux_c (x:xs) | aux_c == column = x == 0  
                                 | otherwise = distint_zero column new_column xs
                                 where new_column = aux_c + 1

check_column :: (Eq a, Num a) => a -> a -> [Bool] -> Bool
check_column column aux_c (x:xs) | aux_c == column = not x 
                                 | otherwise = check_column column new_column xs
                                 where new_column = aux_c + 1

get_columns :: Num a => [[b]] -> a
get_columns (x:xs) = my_length x

my_length  :: Num a =>  [b] -> a
my_length [] = 0 
my_length (x:xs) = my_length xs + 1

get_rows :: Num a =>  [[b]] -> a
get_rows [] = 0 
get_rows (x:xs) = get_rows xs + 1

qsort :: (Num a,Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    let smaller = qsort (filter (<=x) xs)
        larger = qsort (filter (>x) xs)
    in smaller ++ [x] ++ larger

search_ubicated :: (Num a, Ord a) => [[a]] -> [a]
search_ubicated []  = [] 
search_ubicated (x:xs)  =  
    let list = bigger_than_one x
    in list ++ search_ubicated xs

bigger_than_one :: (Num a, Ord a) => [a] -> [a]
bigger_than_one [] = []
bigger_than_one (x:xs) | x > 1 = x:bigger_than_one xs
                        | otherwise = bigger_than_one xs

search_top :: (Ord a, Num a) => [[a]] -> a
search_top [] = 0
search_top (x:xs) | aux > search_top xs = aux
                  | otherwise = search_top xs
                  where aux = list_max x

list_max :: (Ord a,Num a) => [a] -> a
list_max [] = 0
list_max (x:xs) | x > list_max xs = x
                | otherwise = list_max xs

search_number :: (Num a,Ord a) => a -> a -> [[a]] -> (a, a)
search_number _ _ [] = (-1,-1)
search_number num row (x:xs) | fst pos = snd pos
                             | otherwise = search_number num new_row xs 
                            where pos = find num 0 row x
                                  new_row = row + 1 

find :: (Num a, Ord a) => a -> a -> a -> [a] -> (Bool , (a,a))
find _ _ _ [] = (False ,(-1,-1))
find num column row (x:xs) | x == num = (True,(row, column))
                           | otherwise = find num new_column row xs
                           where new_column = column +1