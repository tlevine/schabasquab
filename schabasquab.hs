{-# LANGUAGE DataKinds #-}

-- | Ranges are one-dimensional selectors; they do not contain data.
-- Tuples are ranges of a particular length, with any sort of contained elements.
-- Lists are ranges of unspecified length, with all elements being similar.
-- The cell is the atomic range.
data Sort = Header String
          | Data String

data Range = HTuple [Range] | VTuple [Range]
           | HList  Range | VList Range
           | Cell (String -> Sort)

-- | Grids are two-dimensional data representations.
data Grid = 

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
