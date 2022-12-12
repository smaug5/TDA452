module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.List
import Data.Char

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

ex2 :: Sudoku
ex2 =
    Sudoku
      [ [j 5,j 3,j 4,j 6,j 7,j 8,j 9,j 1,j 2]
      , [j 6,j 7,j 2,j 1,j 9,j 5,j 3,j 4,j 8]
      , [j 1,j 9,j 8,j 3,j 4,j 2,j 5,j 6,j 7]
      , [j 8,j 5,j 9,j 7,j 6,j 1,j 4,j 2,j 3]
      , [j 4,j 2,j 6,j 8,j 5,j 3,j 7,j 9,j 1]
      , [j 7,j 1,j 3,j 9,j 2,j 4,j 8,j 5,j 6]
      , [j 9,j 6,j 1,j 5,j 3,j 7,j 2,j 8,j 4]
      , [j 2,j 8,j 7,j 4,j 1,j 9,j 6,j 3,j 5]
      , [j 3,j 4,j 5,j 2,j 8,j 6,j 1,j 7,j 9]
      ]
      where 
        n = Nothing
        j = Just

-- * A1

-- | allBlankSudoku makes a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 n
  where
    n = Nothing


-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = length(rows sud) == 9 && and rowLength && all isCell cs
  where 
    rs = rows sud
    cs = concat rs
    rowLength = [length row == 9 | row <- rows sud]


isCell :: Cell -> Bool
isCell Nothing = True
isCell (Just i) | i `elem` [1..9] = True 
                | otherwise = False
-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled = all isJust . concat . rows

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStr concatenatedString 
  where
    rowsAsStrings = map (concatMap cellToString) (rows sud)
    concatenatedString = concat $ intersperse "\n" rowsAsStrings

-- Represents Cells with strings instead of Cell
cellToString :: Cell -> String
cellToString Nothing = "."
cellToString (Just i) = show i

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
            content <- readFile file
            let ss = lines content
            let rs = stringToRows ss
            let sud = Sudoku rs
            if (isSudoku sud)
              then return sud
              else return $ error "Not a Sudoku!"
              

stringToRows :: [String] -> [Row]
stringToRows  []                        = []
stringToRows (s:ss) | length s == 9     = map charToCell s:stringToRows ss
                    | otherwise         = error "Doesn't match sudokupattern"


charToCell :: Char -> Cell
charToCell '.'           = Nothing
charToCell c | isDigit c = Just $ digitToInt c
             | otherwise = error "File doesn't cointain a sudoku"
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(1, elements (map Just [1..9])), 
                  (9, return Nothing)]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do 
    sud <-vectorOf 9 $ vectorOf 9 cell -- Gen [Row]
    return $ Sudoku sud

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = length block == 9 && isNotDuplicate block

isNotDuplicate :: Block -> Bool
isNotDuplicate [] = True
isNotDuplicate (Nothing:smallblock) = isNotDuplicate smallblock
isNotDuplicate (cell:smallblock) = notElem cell smallblock &&
                                   isNotDuplicate smallblock


-- * D2

blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ columnBlock sud ++ blockBlocks sud

columnBlock :: Sudoku -> [Block]
columnBlock sud = transpose $ rows sud

blockBlocks :: Sudoku -> [Block]
blockBlocks sud = [blockBlock x y sud | x <- [0..2], y <- [0..2]]

blockBlock :: Int -> Int -> Sudoku -> Block
blockBlock x y sud = concatMap (take 3 . drop (3 * x))
                               ((take 3 . drop (3 * y)) $ rows sud)

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length (blocks sudoku) == 27 && 
                             all (\x -> length x == 9) (blocks sudoku)

-- * D3

isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock (blocks sud)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

-- Makes a list with all the positions of blank cells
blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) = [(r,c) | r <- [0..8], c <- [0..8],
                        isNothing (rows !! r !! c)]


-- checks so that blanks actually return all blank positions
prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = isSudoku allBlankSudoku &&
                        (length (blanks allBlankSudoku) == 81)


-- * E2

-- Changes a cell into a new value, sends error if index is out of bound
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y)
    | null xs = error "List is empty"
    | length xs < i || i < 0 = error "Index out of bounds"
    | otherwise = take i xs ++ [y] ++ drop (i+1) xs


-- checks that the index is valid and not out of bound
validIndex :: [a] -> Int -> Bool
validIndex xs i = length xs <= i || null xs || i <  0

-- checks so that !!= updates a list on the correct position
-- and nothing else
prop_bangBangEquals_correct :: (Int, Int) -> Property
prop_bangBangEquals_correct org (i,y) = not (validIndex org i) ==>
                             length banged == length org && banged !! i == y
     where banged = org !!= (i,y)
     


-- * E3
-- updates a sudoku with a new value in a certain position
update :: Sudoku -> Pos -> Cell -> Sudoku
update sud (row, col) newValue = Sudoku $ rows sud !!= 
                                  (row, rows sud !! row !!= (col, newValue))

-- checks so that update updates a sudoku at the correct position
-- and nothing else
prop_update_updated :: Sudoku -> Pos -> Cell -> Property
prop_update_updated sud (row, col) value = 
  (9 > row && row >= 0) && (9 > col && col >= 0) ==> 
    Sudoku (rows sud !!= (row, newRow)) == update sud (row, col) value
  where newRow = (!!=) (rows sud !! row) (col, value)


------------------------------------------------------------------------------

-- * F1
-- using A2 and D3 (isFilled & isOkay)

-- Solves a sudoku by testing to put in numbers in each blank space
solve :: Sudoku -> Maybe Sudoku
solve sud 
    | null solutions = Nothing
    | otherwise     = Just $ head solutions
  where solutions    = solve' (blanks sud) sud

-- helperfunction for solve by also taking in a list of all blank spaces
solve' :: [Pos] -> Sudoku -> [Sudoku]
solve' [] sud
    | isOkay sud = [sud]
    | otherwise  = []
solve' (pos:positions) sud
    | isOkay sud && isFilled sud        = [sud]
    | isOkay sud && not (isFilled sud)  = concatMap (solve' positions)
                               [update sud pos val | val <- map Just [1..9]]
    | otherwise                         = []


-- * F2

-- reads a sudoku from a file and prints out the solved sudoku
readAndSolve :: FilePath -> IO ()
readAndSolve file = do
    let iosud = readSudoku file
    sud <- iosud
    printSudoku $ fromJust (solve sud)


-- * F3

-- checks if a given sudoku is a solution for an unsolved sudoku
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 = isOkay sud1 && isFilled sud1 && isPartOf sud1 sud2

-- checks that the solved sudoku contains same numbers as
-- an unsolved sudoku
isPartOf :: Sudoku -> Sudoku -> Bool
isPartOf sud1 sud2 = isPartOf' sud1 sud2 (blanks sud2)

-- helperfunction for isPartOf which puts in the same empty spaces
-- as the unsolved sudoku in the solved soduko
isPartOf' :: Sudoku -> Sudoku -> [Pos] -> Bool
isPartOf' sud1 sud2 emptySpaces 
  | null emptySpaces  = sud1 == sud2
  | otherwise         = isPartOf' (update sud1 (head emptySpaces) Nothing)
                            sud2 (tail emptySpaces)

-- * F4
-- checks so that solve acutally makes valid solutions for sudokus
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isJust solvedSud ==>
                      isSolutionOf (fromJust solvedSud) sud
  where solvedSud = solve sud