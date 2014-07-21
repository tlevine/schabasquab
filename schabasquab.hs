{-# LANGUAGE DataKinds #-}

-- | Ranges are one-dimensional.
-- Tuples are ranges of a particular length, with any sort of contained elements.
-- Lists are ranges of unspecified length, with all elements being similar.
-- The cell is the atomic range.

type Cell = String
type HLine = [Cell]
type VLine = [Cell]

data Range = 

data Range = V


{-
data Range = HTuple [Range] | VTuple [Range]
           | HList [Range] | VList [HRange]
           | List Range
           | Cell
-}

main = do putStr "3"
