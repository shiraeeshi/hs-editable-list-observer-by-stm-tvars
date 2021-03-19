module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import ViewUtils (clearScreen, showInRectangle, showInGrid)
import Control.Monad (when)

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
  loop Nothing []
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows
    loop :: Maybe (Int, Int) -> [RowData] -> IO ()
    loop activeCellCoords debugRows = do
      clearScreen
      showInGrid
        (xUpperLeft+16)
        yUpperLeft
        columnCount
        columnWidth
        Nothing
        (map (\row -> [smth row]) debugRows)
      showInGrid
        xUpperLeft
        yUpperLeft
        columnCount
        columnWidth
        activeCellCoords
        (map (\row -> [smth row]) initialRows)
      key <- getKey
      when (key /= "\ESC") $ do
        case key of
          "\ESC[A" -> do -- up
            let newActiveCellCoords =
                  case activeCellCoords of
                    Just (x, y) -> Just (x, max 0 (y-1))
                    Nothing -> Just (0, 0)
            loop newActiveCellCoords ((Row $ "up, " ++ show(fmap snd newActiveCellCoords)):debugRows)
          "\ESC[B" -> do -- down
            let newActiveCellCoords =
                  case activeCellCoords of
                    Just (x, y) -> Just (x, min (rowCount-1) (y+1))
                    Nothing -> Just (0, 0)
            loop newActiveCellCoords ((Row $ "down, " ++ show(fmap snd newActiveCellCoords)):debugRows)
          "\n" -> do -- enter
            return ()
          "q" -> return ()
          _ -> return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
