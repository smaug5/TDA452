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

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 n
  where
    n = Nothing


-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = length(rows sud) == 9 && and [length row == 9 | row <- rows sud] && all isCell cs
  where 
    rs = rows sud
    cs = concat rs


isCell :: Cell -> Bool
isCell Nothing = True
isCell (Just i) | i `elem` [1..9] = True 
                | otherwise = False
-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled = and . map isJust . concat . rows

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStr concatenatedString 
  where
    rowsAsStrings = map (concatMap cellToString) (rows sud)
    concatenatedString = concat $ intersperse "\n" rowsAsStrings
     
cellToString :: Cell -> String
cellToString Nothing = "."
cellToString (Just i) = show i

-- rowToString2 :: [Cell] -> String
-- rowToString2 = concatMap cellToString

-- rowToString :: Row -> String
-- rowToString [] = ""
-- rowToString (c:cs) = cellToString c ++ rowToString cs
-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
            content <- readFile file
            let ss = lines content
            let rs = stringToRows ss
            return $ Sudoku rs


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
isNotDuplicate (cell:smallblock) = notElem cell smallblock && isNotDuplicate smallblock


-- * D2

blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ columnBlock sud ++ blockBlocks sud

columnBlock :: Sudoku -> [Block]
columnBlock sud = transpose $ rows sud

blockBlocks :: Sudoku -> [Block]
blockBlocks sud = [blockBlock x y sud | x <- [0..2], y <- [0..2]]

blockBlock :: Int -> Int -> Sudoku -> Block
blockBlock x y sud = concatMap (take (3) . drop (3 * x)) ((take 3 . drop (3 * y)) $ rows sud)

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

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4