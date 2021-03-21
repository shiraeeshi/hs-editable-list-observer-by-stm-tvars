module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import ViewUtils (clearScreen, showInRectangle, clearRectangle, showInGrid, drawGrid, highlightCell, printFromBottom)
import Control.Monad (when)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad.STM (STM, atomically, retry, throwSTM)
import Control.Concurrent (forkIO)
import Control.Exception (Exception, ErrorCall(ErrorCall), bracket, try)

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
  , debugMessages :: TVar [String]
  , redrawLock :: MVar () }

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
  state@(AppState mainRowsTV highlightedRowIndexTV debugMessagesTV redrawLock) <- do
    mainRows            <- atomically $ newTVar initialRows
    debugMessages       <- atomically $ newTVar ([] :: [String])
    highlightedRowIndex <- atomically $ newTVar Nothing
    redrawLock <- newLock
    return $ AppState mainRows highlightedRowIndex debugMessages redrawLock
  forkIO $ do
    let loop rows = do
          activeCellY <- atomically $ readTVar highlightedRowIndexTV
          let activeCellCoords = fmap (\y -> (0, y)) activeCellY
          bracketInLock redrawLock
            $ showInGrid
                xUpperLeft
                yUpperLeft
                columnCount
                columnWidth
                activeCellCoords
                (map (\row -> [smth row]) rows)
          atomically $ do
            debugMessages <- readTVar debugMessagesTV
            let msg = "main rows listener: updated view"
            writeTVar debugMessagesTV $ take debugLinesCount (msg:debugMessages)
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
          atomically $ do
            debugMessages <- readTVar debugMessagesTV
            let msg = "selected cell listener: updated highlighted cell"
            writeTVar debugMessagesTV $ take debugLinesCount (msg:debugMessages)
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
            $ printFromBottom
                xUpperLeft
                (yUpperLeft+12+debugLinesCount)
                debugMessages
          newDebugMessages <- atomically $ do
            newDebugMessages <- readTVar debugMessagesTV
            if newDebugMessages == debugMessages
              then retry
              else return newDebugMessages
          loop newDebugMessages
    debugMessages <- atomically $ readTVar debugMessagesTV
    loop debugMessages
      
  bracketInLock redrawLock clearScreen
  keepListeningToKeyPresses state
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows
    debugLinesCount = 20
    showEditField appState@(AppState mainRowsTV highlightedRowIndexTV _debugMessagesTV redrawLock) value = do
      let
        txt = "edit cell value:"
        lentxt = length txt
        yPos = 0
        xPos = (columnCount * (columnWidth + 1)) + 3
        replaceNth lst idx val = if idx < 1 then val:(tail lst) else (head lst) : (replaceNth (tail lst) (idx - 1) val)
      showInRectangle xPos yPos lentxt [txt, value]
      key <- getKey
      case key of
        "\n" -> do
          atomically $ do
            maybeCellIndex <- readTVar highlightedRowIndexTV
            rows <- readTVar mainRowsTV
            case maybeCellIndex of
              Nothing -> return ()
              Just cellIndex -> writeTVar mainRowsTV $ replaceNth rows cellIndex (Row value)
          bracketInLock redrawLock $ clearRectangle xPos yPos lentxt 2
          keepListeningToKeyPresses appState
        "\DEL" -> showEditField appState (if (length value) == 0 then value else init value)
        c -> showEditField appState (value ++ c)
    keepListeningToKeyPresses state@(AppState mainRowsTV highlightedRowIndexTV debugMessagesTV _redrawLock) = do
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
                  debugRow = "up, " ++ show(newActiveCellY)
                  newDebugRows = take debugLinesCount (debugRow:debugRows)
              writeTVar highlightedRowIndexTV newActiveCellY
              writeTVar debugMessagesTV newDebugRows
            keepListeningToKeyPresses state
          "\ESC[B" -> do -- down
            atomically $ do
              activeCellY <- readTVar highlightedRowIndexTV
              debugRows <- readTVar debugMessagesTV
              let newActiveCellY =
                    case activeCellY of
                      Just y -> Just $ min (rowCount-1) (y+1)
                      Nothing -> Just 0
                  debugRow = "down, " ++ show(newActiveCellY)
                  newDebugRows = take debugLinesCount (debugRow:debugRows)
              writeTVar highlightedRowIndexTV newActiveCellY
              writeTVar debugMessagesTV newDebugRows
            keepListeningToKeyPresses state
          "\n" -> do -- enter
            --eitherValue <- try $ atomically $ do
            eitherValue <- (try :: IO String -> IO (Either ErrorCall String)) $ atomically $ do
              maybeCellIndex <- readTVar highlightedRowIndexTV
              rows <- readTVar mainRowsTV
              case maybeCellIndex of
                Nothing -> throwSTM (ErrorCall "there's no selected cell")
                Just cellIndex -> do
                  if cellIndex < 0 || cellIndex >= (length rows)
                    then throwSTM (ErrorCall $ "index out of bounds: " ++ (show cellIndex))
                    else return $ smth $ rows !! cellIndex
            case eitherValue of
              Left e -> do
                atomically $ do
                  debugMessages <- readTVar debugMessagesTV
                  let msg = "error: " ++ (show e)
                  writeTVar debugMessagesTV $ take debugLinesCount (msg:debugMessages)
                keepListeningToKeyPresses state
              Right v -> showEditField state v
          "q" -> return ()
          _ -> return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
