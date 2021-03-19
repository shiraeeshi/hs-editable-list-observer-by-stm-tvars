module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import ViewUtils (clearScreen, showInRectangle, showInGrid)
import Control.Monad (when)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Monad.STM (atomically, retry)
import Control.Concurrent (forkIO)

data RowData = Row { smth :: String } deriving Eq

initialRows = [
  Row "something a"
  , Row "something b"
  , Row "something c"
  , Row "something d"
  , Row "something e"
  ]

data AppStateData = AppState
  { mainRows :: TVar [RowData]
  , highlightedRowIndex :: TVar (Maybe Int)
  , debugMessages :: TVar [RowData] }

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  (AppState mainRowsTV highlightedRowIndexTV debugMessagesTV) <- atomically $ do
    mainRows <- newTVar initialRows
    debugMessages <- newTVar ([] :: [RowData])
    highlightedRowIndex <- newTVar Nothing
    return $ AppState mainRows highlightedRowIndex debugMessages
  redrawsTChan <- newTChanIO
  forkIO $ do
    let loop rows activeCellY = do
          let activeCellCoords = fmap (\y -> (0, y)) activeCellY
          atomically
            $ writeTChan redrawsTChan $ showInGrid
                                          xUpperLeft
                                          yUpperLeft
                                          columnCount
                                          columnWidth
                                          activeCellCoords
                                          (map (\row -> [smth row]) rows)
          (newMainRows, newActiveCellY) <- atomically $ do
            newMainRows <- readTVar mainRowsTV
            newActiveCellY <- readTVar highlightedRowIndexTV
            if newMainRows == rows && newActiveCellY == activeCellY
              then retry
              else return (newMainRows, newActiveCellY)
          loop newMainRows newActiveCellY
    (mainRows, activeCellY) <- atomically $ do
      rows <- readTVar mainRowsTV
      y <- readTVar highlightedRowIndexTV
      return (rows, y)
    loop mainRows activeCellY
  forkIO $ do
    let loop debugMessages = do
          atomically
            $ writeTChan redrawsTChan $ showInGrid
                                          xUpperLeft
                                          (yUpperLeft+12)
                                          columnCount
                                          columnWidth
                                          Nothing
                                          (map (\row -> [smth row]) debugMessages)
          newDebugMessages <- atomically $ do
            newDebugMessages <- readTVar debugMessagesTV
            if newDebugMessages == debugMessages
              then retry
              else return newDebugMessages
          loop newDebugMessages
    debugMessages <- atomically $ readTVar debugMessagesTV
    loop debugMessages
      
  clearScreen
  forkIO $ keepListeningToKeyPresses mainRowsTV highlightedRowIndexTV debugMessagesTV
  loopRedraw redrawsTChan
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows
    loopRedraw redrawsTChan = do
      redrawToPerform <- atomically $ readTChan redrawsTChan
      redrawToPerform
      loopRedraw redrawsTChan
    keepListeningToKeyPresses mainRowsTV highlightedRowIndexTV debugMessagesTV = do
      key <- getKey
      when (key /= "\ESC") $ do
        case key of
          "\ESC[A" -> do -- up
            atomically $ do
              activeCellY <- readTVar highlightedRowIndexTV
              debugRows <- readTVar debugMessagesTV
              let newActiveCellY =
                    case activeCellY of
                      Just y -> Just $ max 0 (y-1)
                      Nothing -> Just 0
                  debugRow = Row $ "up, " ++ show(newActiveCellY)
                  newDebugRows = take 5 (debugRow:debugRows)
              writeTVar highlightedRowIndexTV newActiveCellY
              writeTVar debugMessagesTV newDebugRows
            keepListeningToKeyPresses mainRowsTV highlightedRowIndexTV debugMessagesTV
          "\ESC[B" -> do -- down
            atomically $ do
              activeCellY <- readTVar highlightedRowIndexTV
              debugRows <- readTVar debugMessagesTV
              let newActiveCellY =
                    case activeCellY of
                      Just y -> Just $ min (rowCount-1) (y+1)
                      Nothing -> Just 0
                  debugRow = Row $ "down, " ++ show(newActiveCellY)
                  newDebugRows = take 5 (debugRow:debugRows)
              writeTVar highlightedRowIndexTV newActiveCellY
              writeTVar debugMessagesTV newDebugRows
            keepListeningToKeyPresses mainRowsTV highlightedRowIndexTV debugMessagesTV
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
