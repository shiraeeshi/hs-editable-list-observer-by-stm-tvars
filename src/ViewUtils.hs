module ViewUtils
 ( showInRectangle
 , showInGrid
 , clearScreen
 ) where

import Control.Monad (forM_)
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
        putStr cellValue
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
    rowBoxStr      = "│" ++ (intercalate "│" (replicate columnCount (replicate columnWidth ' '))) ++ "│"
    betweenRowsStr = "├" ++ (intercalate "┼" (replicate columnCount (replicate columnWidth '─'))) ++ "┤"
    bottomStr      = "└" ++ (intercalate "┴" (replicate columnCount (replicate columnWidth '─'))) ++ "┘"
    printRowBox rowIndex = do
      if rowIndex == 0
        then do
          setCursorPosition (y0+1) x0
          putStr rowBoxStr
        else do
          setCursorPosition (y0+rowIndex*2) x0
          putStr betweenRowsStr
          setCursorPosition (y0+rowIndex*2+1) x0
          putStr rowBoxStr
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
  setCursorPosition yPos xPosLeft
  putStr topStr
  setCursorPosition (yPos+2) xPosLeft
  putStr bottomStr
  setCursorPosition (yPos+1) xPosLeft
  putStr "┃"
  setCursorPosition (yPos+1) xPosRight
  putStr "┃"
