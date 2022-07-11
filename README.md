# hidato-sudoku

## Class

### Direction

```haskell
data Direction = Up | UpperRightDiagonal | 
                 Right' | LowerRightDiagonal | 
                 Down | LowerLeftDiagonal | 
                 Left' | UpperLeftDiagonal  
                 deriving (Eq, Enum, Bounded, Show)
```
### Direction

```haskell
data Hidato = Hidato { 
            matrix :: [[Int]], 
            mask :: [[Bool]], 
            freeCells :: Int, 
            amountRows :: Int, 
            amountCols :: Int 
        } | Nil deriving (Eq)
```
