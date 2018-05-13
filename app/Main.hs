module Main where

import Lib
import UI.NCurses

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 1 10
        drawString "Best To Do App Ever!"
        moveCursor 2 10
        drawString "===================="
        moveCursor 4 1
        drawString "1. List events"
        moveCursor 5 1
        drawString "2. List deadline items"
        moveCursor 6 1
        drawString "3. Enter new event"
        moveCursor 7 1
        drawString "4. Enter new deadline item"
        moveCursor 10 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
