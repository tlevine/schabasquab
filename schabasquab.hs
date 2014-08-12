{-# LANGUAGE DataKinds #-}

data Box a = Box [[a]]
type Cell = String
type Grid = Box Cell
type CellSpec = Cell -> Bool
data GridSpec = GridSpec (Box CellSpec)

-- | Construct a box. Check that the component rows are all of the same length.
assembleBox :: [[a]] -> Maybe Box a
assembleBox listing = Box listing

-- | The width of a box
width :: Box a -> Int
width (Box row1:_) = length row1

-- | The height of a box
height :: Box a -> Int
height (Box box) = length box

dropColumns :: Int -> Box a -> Box a
dropColumns n (Box box) = assembleBox $ map (drop n) box

dropRows :: Int -> Box a -> Box a
dropRows n (Box box) = assembleBox $ drop n box

takeColumns :: Int -> Box a -> Box a
takeColumns n (Box box) = assembleBox $ map (take n) box

takeRows :: Int -> Box a -> Box a
takeRows n (Box box) = assembleBox $ take n box

-- | Grammars are one-dimensional, but they describe two-dimensional grids.
-- Tuples are grammars of a particular length, with any specified sort of contained elements.
-- Lists are grammars of unspecified length, with all elements being similar.
-- The gridspec is the atomic grammar.
data Grammar = HTuple [Grammar] | VTuple [Grammar]
             | HList   Grammar  | VList   Grammar
             | Atom GridSpec


-- | Does a particular grid follow a particular grammar?
matches :: Grammar -> Grid -> Bool
matches (HTuple g:gs) grid = matches g grid && matches gs grid
matches (VTuple g:gs) grid = matches g grid && matches gs grid
matches (HList grammar) grid = matches grammar (takeColumns w grid) && matches grammar (dropColumns w grid)
  where
    w = width grammar
matches (VList grammar) grid = matches grammar (takeRows h grid) && matches grammar (dropRows h grid)
  where
    h = height grammar
matches (Atom (GridSpec gridspec)) grid
  | not (validDimensions = width gridspec == width grid && height gridspec == height grid) = False
  | height gridspec == 0 = True
  | height gridspec == 1 = (head (head gridspec)) (head (head grid)) && validate (tail (head gridspec)) (tail (head grid))
  | otherwise = validate (tail gridspec) (tail grid)







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
