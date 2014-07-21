{-# LANGUAGE DataKinds #-}

-- | Ranges are one-dimensional selectors; they do not contain data.
-- Tuples are ranges of a particular length, with any sort of contained elements.
-- Lists are ranges of unspecified length, with all elements being similar.
-- The cell is the atomic range.
data Range = HTuple [Range] | VTuple [Range]
           | HList  Range | VList Range
           | Cell String

-- | Grids are two-dimensional data representations.
data Grid = 

-- | Unpack one level of a range.
unpack :: Grid -> Range -> (Maybe String, [Range])
unpack grid (HTuple range:ranges) = unpack (left (length range) grid) (HTuple ranges)
unpack grid (VTuple range:ranges) = unpack (down (length range) grid) (VTuple ranges)
unpack grid (HList range) = (
unpack grid (VList range) = 
unpack grid (Cell value) = (Just value, [])

tidytable :: Range
tidytable = 
type TidyTable =
HList (VTuple [Cell, VList])

main = do putStr "3"
