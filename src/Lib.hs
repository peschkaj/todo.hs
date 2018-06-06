module Lib
    ( someFunc,
      displayEandD
    ) where

import Events
import Deadline
import Data.Time.LocalTime

someFunc :: IO ()
someFunc = putStrLn "someFunc"


{- will convert the todo with a sooner startTime to it's string form-}
sortEandD :: Event -> Deadline -> TimeZone -> String
sortEandD e d tz = if (startTime e) < (dueDate d)
                    then formatEvent tz e
                else show d
 
{- modifies the given list of events and deadlines, based on which list has the todo with the sooner startTime-}
modifyEandD :: [Event] -> [Deadline] -> ([Event],[Deadline])
modifyEandD (e:es) (d:ds) = if (startTime e) < (dueDate d)
                                then (es, (d:ds))
                            else ((e:es), ds)

{- displays all events and deadlines in sorted order based on each todo item's startTime-}
displayEandD :: IO ()
displayEandD = do es <- getCurrentEvents
                  ds <- getCurrentDeadlines
                  tz <- getCurrentTimeZone
                  putStrLn (displayAll (sortEvents es) (sortDeadlines ds) tz)

{- Given a list of events and deadlines this function converts all of the items into a sorted string containing each item's data -}
displayAll :: [Event] -> [Deadline] -> TimeZone -> String
displayAll [] [] tz = "no events or deadlines"
displayAll [] ds tz = deadlinesToLines ds
displayAll es [] tz = eventsToString tz es
displayAll (e:es) (d:ds) tz = (sortEandD e d tz) ++ (displayAll (fst (modifyEandD (e:es) (d:ds))) (snd (modifyEandD (e:es) (d:ds))) tz)
