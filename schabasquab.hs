{-# LANGUAGE DataKinds #-}

-- | Ranges are one-dimensional.
-- Tuples are ranges of a particular length, with any sort of contained elements.
-- Lists are ranges of unspecified length, with all elements being similar.
-- The cell is the atomic range.
data Range = HTuple [Range] | VTuple [Range]
           | HList  [Range] | VList  [Range]
           | Cell String

-- | Unpack one level of a range.
unpack :: Range -> Range -> [Range]
unpack grid (HTuple range:ranges) = ((left (length range) grid) range)
unpack grid (VTuple range:ranges) = ((down (length range) grid) range)
unpack grid (HList range) = 
unpack grid (VList range) = 
unpack grid (Cell value) = (value, [])

main = do putStr "3"
