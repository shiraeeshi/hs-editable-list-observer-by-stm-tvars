module ViewUtils
 ( showInRectangle
 , clearRectangle
 , showInGrid
 , clearScreen
 , drawGrid
 , highlightCell
 , printFromBottom
 ) where

import Control.Monad (when, forM_)
import System.Console.ANSI
import Data.List (intercalate)


showInRectangle :: Int -> Int -> Int -> [String] -> IO ()
showInRectangle xPos yPos width rows = do
  let
    topStr    = "┌" ++ (replicate width '─') ++ "┐"
    middleStr = "│" ++ (replicate width ' ') ++ "│"
    bottomStr = "└" ++ (replicate width '─') ++ "┘"
  saveCursor
  setCursorPosition yPos xPos
  putStr topStr
  forM_ (rows `zip` [1..]) $ \(row, rownum) -> do
    setCursorPosition (yPos+rownum) xPos
    putStr middleStr
    setCursorPosition (yPos+rownum) (xPos+1)
    putStr row
  setCursorPosition (yPos + (length rows) + 1) xPos
  putStr bottomStr
  restoreCursor

clearRectangle :: Int -> Int -> Int -> Int -> IO ()
clearRectangle xPos yPos width rowsCount = do
  let emptyStr = replicate (width + 2) ' '
  saveCursor
  forM_ [0..(rowsCount+1)] $ \y -> do
    setCursorPosition (yPos + y) xPos
    putStr emptyStr
  restoreCursor

printFromBottom :: Int -> Int -> [String] -> IO ()
printFromBottom xLeftBottom yLeftBottom strs = do
  saveCursor
  forM_ (strs `zip` [0..]) $ \(str, idx) -> do
    setCursorPosition (yLeftBottom - idx) xLeftBottom
    clearLine
    putStr str
  restoreCursor

showInGrid :: Int -> Int -> Int -> Int -> Maybe (Int, Int) -> [[String]] -> IO ()
showInGrid xUpperLeft yUpperLeft columnCount columnWidth activeCellCoords cellsData = do
  let
    x0 = xUpperLeft
    y0 = yUpperLeft
    rowCount = length cellsData
    printRowValues row rowIndex = do
      let
        yPos = yUpperLeft+1+rowIndex*2
      forM_ (row `zip` [0..]) $ \(cellValue, cellIndex) -> do
        setCursorPosition yPos (xUpperLeft + 1 + (columnWidth+1)*cellIndex)
        putStr (take columnWidth (cellValue ++ (repeat ' ')))
  saveCursor
  drawGrid xUpperLeft yUpperLeft columnWidth columnCount rowCount
  forM_ activeCellCoords $ highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount
  forM_ (cellsData `zip` [0..]) $ \(row, rowIndex) -> do
    printRowValues row rowIndex
  restoreCursor

drawGrid :: Int -> Int -> Int -> Int -> Int -> IO ()
drawGrid xUpperLeft yUpperLeft columnWidth columnCount rowCount = do
  let
    x0 = xUpperLeft
    y0 = yUpperLeft
    topStr         = "┌" ++ (intercalate "┬" (replicate columnCount (replicate columnWidth '─'))) ++ "┐"
    betweenRowsStr = "├" ++ (intercalate "┼" (replicate columnCount (replicate columnWidth '─'))) ++ "┤"
    bottomStr      = "└" ++ (intercalate "┴" (replicate columnCount (replicate columnWidth '─'))) ++ "┘"
    printRowBox rowIndex = do
      when (rowIndex /= 0) $ do
        setCursorPosition (y0+rowIndex*2) x0
        putStr betweenRowsStr
      let yPos = y0 + rowIndex*2 + 1
      forM_ [x0,(x0+columnWidth+1)..(x0+(columnWidth+1)*columnCount)] $ \x -> do
        setCursorPosition yPos x
        putStr "│"
  saveCursor
  setCursorPosition yUpperLeft xUpperLeft
  putStr topStr
  forM_ [0 .. rowCount - 1] printRowBox
  setCursorPosition (yUpperLeft+rowCount*2) xUpperLeft
  putStr bottomStr
  restoreCursor

highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount (activeCellX, activeCellY) = do
  let
    yPos = yUpperLeft + activeCellY*2
    xPosLeft = xUpperLeft + (columnWidth+1)*activeCellX
    xPosRight = xPosLeft + 1 + columnWidth
    leftUpperCorner = if activeCellX == 0
                        then
                          if activeCellY == 0 then "┏" else "┢"
                        else
                          if activeCellY == 0 then "┲" else "╆"
    leftBottomCorner = if activeCellX == 0
                         then
                           if activeCellY == rowCount - 1 then "┗" else "┡"
                         else
                           if activeCellY == rowCount - 1 then "┺" else "╄"
    rightUpperCorner = if activeCellX == (columnCount-1)
                         then
                           if activeCellY == 0 then "┓" else "┪"
                         else
                           if activeCellY == 0 then "┱" else "╅"
    rightBottomCorner = if activeCellX == (columnCount-1)
                          then
                            if activeCellY == rowCount - 1 then "┛" else "┩"
                          else
                            if activeCellY == rowCount - 1 then "┹" else "╃"
    topStr = leftUpperCorner ++ (replicate columnWidth '━') ++ rightUpperCorner
    bottomStr = leftBottomCorner ++ (replicate columnWidth '━') ++ rightBottomCorner
  saveCursor
  setCursorPosition yPos xPosLeft
  putStr topStr
  setCursorPosition (yPos+2) xPosLeft
  putStr bottomStr
  setCursorPosition (yPos+1) xPosLeft
  putStr "┃"
  setCursorPosition (yPos+1) xPosRight
  putStr "┃"
  restoreCursor
