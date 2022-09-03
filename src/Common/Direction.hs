module Common.Direction
    ( 
        Direction(..),
        directionToRow,
        directionToCol  
    ) where

data Direction = Up | UpperRightDiagonal | 
                 Right' | LowerRightDiagonal | 
                 Down | LowerLeftDiagonal | 
                 Left' | UpperLeftDiagonal  
                 deriving (Eq, Enum, Bounded, Show)


directionToRow :: Direction -> Int
directionToRow direction = case direction of
  Up -> -1
  UpperRightDiagonal -> -1
  Right' -> 0
  LowerRightDiagonal -> 1
  Down -> 1
  LowerLeftDiagonal -> 1
  Left' -> 0
  UpperLeftDiagonal -> -1


directionToCol :: Direction -> Int
directionToCol direction = case direction of
    Up -> 0
    UpperRightDiagonal -> 1
    Right' -> 1
    LowerRightDiagonal -> 1
    Down -> 0
    LowerLeftDiagonal -> -1
    Left' -> -1
    UpperLeftDiagonal -> -1