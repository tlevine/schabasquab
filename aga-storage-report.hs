{-# LANGUAGE OverloadedStrings #-}
module Read where
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens

filename = "examples/scott_neal__37897__Aga4-7.xlsx"

main :: IO ()
main = do
  bs <- L.readFile filename
  let value = toXlsx bs ^? ixSheet "List1" .
              ixCell (3,2) . cellValue . _Just
  putStrLn $ "Cell B3 contains " ++ show value

