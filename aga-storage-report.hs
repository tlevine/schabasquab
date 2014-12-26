{-# LANGUAGE OverloadedStrings #-}
module Read where
import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import Control.Lens

filename = "examples/scott_neal__37897__Aga4-7.xlsx"

main :: IO ()
main = do
  bs <- L.readFile filename
  let value = toXlsx bs ^? ixSheet "AGA Storage" .
              ixCell (3,1) . cellValue . _Just
  putStrLn $ "Cell A3 contains " ++ show value

