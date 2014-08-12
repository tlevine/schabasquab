{-# LANGUAGE DataKinds #-}

data Box a = Box [[a]]
type Cell = String
type Grid = Box Cell
type CellSpec = Cell -> Bool
data GridSpec = GridSpec (Box CellSpec)

-- | Construct a box. Check that the component rows are all of the same length.
box :: [[a]] -> Maybe Box a
box listing = Box listing

-- | The width of a box
width :: Box a -> Int
width row1:_ = length row1

-- | The height of a box
height :: Box a -> Int
height box = length box

-- | Grammars are one-dimensional, but they describe two-dimensional grids.
-- Tuples are ranges of a particular length, with any sort of contained elements.
-- Lists are ranges of unspecified length, with all elements being similar.
-- The cell is the atomic range.
data Grammar = HTuple [Grammar] | VTuple [Grammar]
             | HList  [Grammar] | VList [Grammar]
             | GridSpec [[CellSpec]]




matches :: Grammar -> Grid -> Bool
matches (GridSpec gridspec) grid = 










type Box = (Grid, Range)

-- | Boxes contain either more boxes or values
data Unpack = Boxes [Box] | 

-- | Unpack one level of a range.
unpack :: (Grid, Range) -> [(Grid, Range)]
unpack (grid, (HTuple range:ranges)) = (unpack (keepLeft (length range)) grid) ++ (unpack (dropLeft (length range) grid) (HTuple ranges))
unpack (grid, (VTuple range:ranges)) = (unpack (keepDown (length range)) grid) ++ (unpack (dropDown (length range) grid) (VTuple ranges))
unpack (grid, (HList range)) = (
unpack (grid, (VList range)) = 
unpack (grid, (Cell sorter)) = sorter $ value (keepLeft 1 grid $ keepDown 1 grid)

tidytable :: Range
tidytable = 
type TidyTable =
HList (VTuple [Cell, VList])


validate :: Grammar -> Box -> Bool
validate (Box grammar) box
  | height grammar == height box && width box == width box = all $ mapGrid validate box
  | otherwise = False
validate (LeftFill grammar) bigBox
  | height grammar == height box = validate grammar box && validate rest
  | width grammar == 0 || height fill == 0 = True
  | otherwise = False
  where
    box, rest = splitFill grammar bigBox
validate (DownFill grammar) bigBox
  | width grammar == width grid = validate grammar box && validate rest
  | width grammar == 0 || height fill == 0 = True
  | otherwise = False
  where
    box, rest = splitFill grammar bigBox

splitFill :: Fill -> Box -> (Box, Fill)
splitFill (LeftFill fill) box = takeLeft (width box) fill
splitFill (DownFill fill) box = takeDown (width box) fill

main = do putStr "3"
