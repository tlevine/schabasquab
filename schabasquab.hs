{-# LANGUAGE DataKinds #-}

-- | Ranges are one-dimensional.
-- Tuples are ranges of a particular length, with any sort of contained elements.
-- Lists are ranges of unspecified length, with all elements being similar.
-- The cell is the atomic range.

data Range = HTuple [Range] | VTuple [Range]
           | HList [Range] | VList [Range]
           | Cell

main = do putStr "3"
