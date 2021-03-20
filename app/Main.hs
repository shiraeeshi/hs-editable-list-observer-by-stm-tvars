module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import ViewUtils (clearScreen, showInRectangle, showInGrid, drawGrid, highlightCell)
import Control.Monad (when)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad.STM (STM, atomically, retry)
import Control.Concurrent (forkIO)
import Control.Exception (bracket)

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

newLock :: IO (MVar ())
newLock = newEmptyMVar

lock :: MVar () -> IO ()
lock v = putMVar v ()

unlock :: MVar () -> IO ()
unlock v = takeMVar v

bracketInLock :: MVar () -> IO () -> IO ()
bracketInLock l action =
  bracket
    (lock l)
    (\_ -> unlock l)
    $ \_ -> action

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  (AppState mainRowsTV highlightedRowIndexTV debugMessagesTV) <- atomically $ do
    mainRows <- newTVar initialRows
    debugMessages <- newTVar ([] :: [RowData])
    highlightedRowIndex <- newTVar Nothing
    return $ AppState mainRows highlightedRowIndex debugMessages
  redrawLock <- newLock
  forkIO $ do
    let loop rows = do
          let activeCellCoords = Nothing
          bracketInLock redrawLock
            $ showInGrid
                xUpperLeft
                yUpperLeft
                columnCount
                columnWidth
                activeCellCoords
                (map (\row -> [smth row]) rows)
          newMainRows <- atomically $ do
            newMainRows <- readTVar mainRowsTV
            if newMainRows == rows
              then retry
              else return newMainRows
          loop newMainRows
    mainRows <- atomically $ do
      rows <- readTVar mainRowsTV
      return rows
    loop mainRows
  forkIO $ do
    let loop activeCellY = do
          let activeCellCoords = fmap (\y -> (0, y)) activeCellY
          bracketInLock redrawLock
            $ do
                drawGrid xUpperLeft yUpperLeft columnWidth columnCount rowCount
                case activeCellCoords of
                  Nothing -> return ()
                  Just coordsPair -> highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount coordsPair
          newActiveCellY <- atomically $ do
            newActiveCellY <- readTVar highlightedRowIndexTV
            if newActiveCellY == activeCellY
              then retry
              else return newActiveCellY
          loop newActiveCellY
    activeCellY <- atomically $ readTVar highlightedRowIndexTV
    loop activeCellY
  forkIO $ do
    let loop debugMessages = do
          bracketInLock redrawLock
            $ showInGrid
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
      
  bracketInLock redrawLock clearScreen
  keepListeningToKeyPresses mainRowsTV highlightedRowIndexTV debugMessagesTV
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows
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
