{-# LANGUAGE DataKinds #-}

-- | Ranges are one-dimensional selectors; they do not contain data.
-- Tuples are ranges of a particular length, with any sort of contained elements.
-- Lists are ranges of unspecified length, with all elements being similar.
-- The cell is the atomic range.
data Range = HTuple [Range] | VTuple [Range]
           | HList  [Range] | VList  [Range]
           | Cell

-- | Grids are two-dimensional data representations.
data Grid = 

-- | Unpack one level of a range.
unpack :: (Grid, Range) -> (Grid, [Range])
unpack grid (HTuple range:ranges) = (left (length range) grid, ranges)
unpack grid (VTuple range:ranges) = (down (length range) grid, ranges)
unpack grid (HList range) = (
unpack grid (VList range) = 
unpack grid (Cell) = (left 1 $ down 1 $ grid, [])

main = do putStr "3"
