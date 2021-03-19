module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import ViewUtils (showInRectangle, showInGrid)
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
  loop Nothing
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    loop :: Maybe (Int, Int) -> IO ()
    loop activeCellCoords = do
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
                    Just (x, y) -> Just (x, y-1)
                    Nothing -> Just (0, 0)
            loop newActiveCellCoords
          "\ESC[B" -> do -- down
            let newActiveCellCoords =
                  case activeCellCoords of
                    Just (x, y) -> Just (x, y+1)
                    Nothing -> Just (0, 0)
            loop newActiveCellCoords
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
