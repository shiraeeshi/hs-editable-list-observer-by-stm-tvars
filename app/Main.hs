module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import ViewUtils (showInRectangle, showInGrid)

data RowData = Row { smth :: String }

initialRows = [
  Row "something a"
  , Row "something b"
  , Row "something c"
  , Row "something d"
  , Row "something e"
  ]

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  let
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    activeCellCoords = Nothing
  showInGrid
    xUpperLeft
    yUpperLeft
    columnCount
    columnWidth
    activeCellCoords
    (map (\row -> [smth row]) initialRows)

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
