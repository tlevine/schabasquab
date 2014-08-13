{-# LANGUAGE DataKinds #-}


type CellValue = String
type CellSpec = String -> Bool
type Box a = [[a]]

-- | Grammars are one-dimensional, but they describe two-dimensional grids.
-- Tuples are grammars of a particular length, with any specified sort of contained elements.
-- Lists are grammars of unspecified length, with all elements being similar.
-- The gridspec is the atomic grammar.
data Grammar = HTuple [Grammar] | VTuple [Grammar]
             | HList   Grammar  | VList   Grammar
             | Atom (Box CellSpec)


{-
-- | Construct a box. Check that the component rows are all of the same length.
assembleBox :: a => [[a]] -> Maybe (Box a)
assembleBox (firstRow:rows)
  | all (\row -> length row == length firstRow) rows = Just (firstRow:rows)
  | otherwise = Nothing
-}

-- | The width of a box
width :: Box a -> Int
width (row1:_) = length row1

-- | The height of a box
height :: Box a -> Int
height = length

-- | Remove columns from the left
dropColumns :: Int -> Box a -> Box a
dropColumns n (box) = map (drop n) box

-- | Remove rows from the top
dropRows :: Int -> Box a -> Box a
dropRows = drop

-- | Keep columns from the left
takeColumns :: Int -> Box a -> Box a
takeColumns n box = map (take n) box

-- | Keep rows from the top
takeRows :: Int -> Box a -> Box a
takeRows = take

-- | Does a particular grid follow a particular grammar?
matches :: Grammar -> Box CellValue -> Bool
--matches (HTuple g:gs) grid = matches g grid && matches gs grid
--matches (VTuple g:gs) grid = matches g grid && matches gs grid
matches (HList grammar) grid = matches grammar (takeColumns w grid) && matches grammar (dropColumns w grid)
  where
    w = width grammar
matches (VList grammar) grid = matches grammar (takeRows h grid) && matches grammar (dropRows h grid)
  where
    h = height grammar
{-
matches (Atom (GridSpec gridspec)) grid
  | not (validDimensions = width gridspec == width grid && height gridspec == height grid) = False
  | height gridspec == 0 = True
  | height gridspec == 1 = (head (head gridspec)) (head (head grid)) && validate (tail (head gridspec)) (tail (head grid))
  | otherwise = validate (tail gridspec) (tail grid)
-}

main = do putStr "3"
