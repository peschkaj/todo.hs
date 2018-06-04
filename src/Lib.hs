module Lib
    ( someFunc,
      displayEandD
    ) where

import Events
import Deadline
import Data.Time.LocalTime

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sortEandD :: Event -> Deadline -> TimeZone -> String
sortEandD e d tz = if (startTime e) < (dueDate d)
                    then formatEvent tz e
                else show d
 
modifyEandD :: [Event] -> [Deadline] -> ([Event],[Deadline])
modifyEandD (e:es) (d:ds) = if (startTime e) < (dueDate d)
                                then (es, (d:ds))
                            else ((e:es), ds)
displayEandD :: IO ()
displayEandD = do es <- getCurrentEvents
                  ds <- getCurrentDeadlines
                  tz <- getCurrentTimeZone
                  putStrLn (displayAll (sortEvents es) (sortDeadlines ds) tz)


displayAll :: [Event] -> [Deadline] -> TimeZone -> String
displayAll [] [] tz = "no events or deadlines"
displayAll [] ds tz = deadlinesToLines ds
displayAll es [] tz = eventsToString tz es
displayAll (e:es) (d:ds) tz = (sortEandD e d tz) ++ (displayAll (fst (modifyEandD (e:es) (d:ds))) (snd (modifyEandD (e:es) (d:ds))) tz)
