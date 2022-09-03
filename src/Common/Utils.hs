module Common.Utils 
    (
        alreadyUbicated,
        correctlyPlaced,
        changeState,
        changeRow,
        notValidHidato,
        notValidMask,
        checkHidato,
        checkMask,
        distintZero,
        checkColumn,
        getColumns,
        getRows,
        myLength,
        qsort,
        searchUbicated,
        biggerThanOne, 
        searchTop, 
        listMax, 
        searchNumber, 
        find
    ) where


alreadyUbicated :: (Num a,Ord a) => a -> a -> Bool
alreadyUbicated num x | x == num = True
                       | otherwise = False

correctlyPlaced :: (Num a, Ord a ) => a -> a -> a -> [[a]] -> Bool
correctlyPlaced num row column hidato | row + 1 == row_pos && column + 1 == column_pos = True
                                       | row - 1 == row_pos && column + 1 == column_pos = True
                                       | row == row_pos && column + 1 == column_pos = True
                                       | row == row_pos && column - 1 == column_pos = True
                                       | row - 1 == row_pos && column - 1 == column_pos = True
                                       | row + 1 == row_pos && column - 1 == column_pos = True
                                       | row - 1 == row_pos && column == column_pos = True
                                       | row + 1 == row_pos && column == column_pos = True
                                       | otherwise = False
                                       where pos = searchNumber num 0 hidato
                                             row_pos = fst pos
                                             column_pos = snd pos

changeState :: (Num a,Eq a) => a -> a -> a -> a -> [[a]] -> [[a]]
changeState num row column aux_r (x:xs) | row == aux_r = changeRow num column 0 x : xs
                                         | otherwise = x : changeState num row column new_row xs
                                         where new_row = aux_r + 1  

changeRow :: (Num a,Eq a) => a-> a -> a -> [a] -> [a]
changeRow num column aux_c (x:xs) | column == aux_c =  num : xs
                                        | otherwise = x : changeRow num column new_column xs
                                         where new_column = aux_c + 1 

notValidHidato :: (Num a, Eq a) => a -> a -> [[a]] -> Bool
notValidHidato row column hidato | checkHidato row column 0 hidato = False
                                      | otherwise = True

notValidMask :: (Num a, Eq a) => a -> a -> [[Bool]] -> Bool
notValidMask row column mask | getRows mask == row = True
                                  | row == -1 = True
                                  | column == -1 = True
                                  | column == getColumns mask = True
                                  | checkMask row column 0 mask = True
                                  | otherwise = False

checkHidato :: (Eq a,Num a) => a -> a -> a -> [[a]] -> Bool
checkHidato row column aux_r (x:xs) | row == aux_r = distintZero column 0 x
                                     | otherwise = checkHidato row column new_row xs 
                                     where new_row = aux_r + 1

checkMask :: (Eq a,Num a) => a -> a -> a -> [[Bool]] -> Bool
checkMask row column aux_r (x:xs) | row == aux_r = checkColumn column 0 x
                                   | otherwise = checkMask row column new_row xs 
                                   where new_row = aux_r + 1

distintZero :: (Eq a, Num a) => a -> a -> [a] -> Bool
distintZero column aux_c (x:xs) | aux_c == column = x == 0  
                                 | otherwise = distintZero column new_column xs
                                 where new_column = aux_c + 1

checkColumn :: (Eq a, Num a) => a -> a -> [Bool] -> Bool
checkColumn column aux_c (x:xs) | aux_c == column = not x 
                                 | otherwise = checkColumn column new_column xs
                                 where new_column = aux_c + 1

getColumns :: Num a => [[b]] -> a
getColumns (x:xs) = myLength x

myLength  :: Num a =>  [b] -> a
myLength [] = 0 
myLength (x:xs) = myLength xs + 1

getRows :: Num a =>  [[b]] -> a
getRows [] = 0 
getRows (x:xs) = getRows xs + 1

qsort :: (Num a,Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
    let smaller = qsort (filter (<=x) xs)
        larger = qsort (filter (>x) xs)
    in smaller ++ [x] ++ larger

searchUbicated :: (Num a, Ord a) => [[a]] -> [a]
searchUbicated []  = [] 
searchUbicated xs = concatMap biggerThanOne xs

biggerThanOne :: (Num a, Ord a) => [a] -> [a]
biggerThanOne [] = []
biggerThanOne (x:xs) | x > 1 = x:biggerThanOne xs
                     | otherwise = biggerThanOne xs

searchTop :: (Ord a, Num a) => [[a]] -> a
searchTop [] = 0
searchTop (x:xs) | aux > searchTop xs = aux
                  | otherwise = searchTop xs
                  where aux = listMax x

listMax :: (Ord a,Num a) => [a] -> a
listMax [] = 0
listMax (x:xs) | x > listMax xs = x
                | otherwise = listMax xs

searchNumber :: (Num a,Ord a) => a -> a -> [[a]] -> (a, a)
searchNumber _ _ [] = (-1,-1)
searchNumber num row (x:xs) | fst pos = snd pos
                             | otherwise = searchNumber num new_row xs 
                            where pos = find num 0 row x
                                  new_row = row + 1 

find :: (Num a, Ord a) => a -> a -> a -> [a] -> (Bool , (a,a))
find _ _ _ [] = (False ,(-1,-1))
find num column row (x:xs) | x == num = (True,(row, column))
                           | otherwise = find num new_column row xs
                           where new_column = column +1