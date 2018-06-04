{-#  LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Events where

import           Data.Aeson
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import           Data.Time.Clock      (NominalDiffTime, UTCTime, addUTCTime,
                                       getCurrentTime)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime
import           GHC.Generics

{-data ToDo = Deadline { title :: String, description :: String, dueDate :: UTCTime} |
                Event { name :: String, detail :: String, startTime :: UTCTime, endTime :: UTCTime}
                 deriving (Show, Eq, Generic, ToJSON, FromJSON) -}

data Event = Event { name :: String, detail :: String, startTime :: UTCTime, endTime :: UTCTime}
                 deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- Testing events


import Data.Time
:set -XOverloadedStrings
t <- getCurrentTime
tz <- getCurrentTimeZone

-- TEST#1
e1 = Event "First Event" "Some details" (addMinutes 2000 t) (addMinutes 2030 t)
e2 = Event "Second Event" "Some other details" (addMinutes 30 t) (addMinutes 180 t)

es <- addEvent e1 ([] :: [Event])
es' <- addEvent e2 es

es'' <- getCurrentEvents

-- Are the lists the same?
es'' == es'

--the result should be true

-- TEST#2

e1 = Event "First Event" "Some details" (addMinutes 2000 t) (addMinutes 2030 t)
e2 = Event "Second Event" "Some other details" (addMinutes 2010 t) (addMinutes 2040 t)

es <- addEvent e1 ([] :: [Event])
es' <- addEvent e2 es

-- check Events file, e2 should NOT be there

-}

eventFile :: String
eventFile = "Events"

-- read from event file
getCurrentEvents :: IO [Event]
getCurrentEvents = decodeFileStrict eventFile >>= (\x -> case x of
                        (Just ts) -> return ts
                        Nothing   -> return [])


-- write the new Event to the Events file if it does not overlap with existing event, otherwise 
-- do not add and display error message
addEvent :: Event -> [Event] -> IO ()
addEvent x es = do encodeFile eventFile (snd result)
                   if (fst result) then
                      putStrLn ("\nEvent added to Schedule successfully.\n")
                   else
                      putStrLn ("\nSorry, cannot add Event. The Event you have entered overlaps with an existing event.\n")
           where result = (listOfEvents x es)


-- adds the new event to the list of events if it does not overlap with any existing events
listOfEvents :: Event -> [Event] -> (Bool, [Event])
listOfEvents x es = if (existingEvent x es) then (False,es) else (True,(x:es))


-- checks if the new event overlaps with any existing events in the list
existingEvent :: Event -> [Event] -> Bool
existingEvent x [] = False
existingEvent x (e:es) =  if (startTime x) < (endTime e) && (startTime e) < (endTime x)
                             then True
                          else (existingEvent x es)


sortEvents :: [Event] -> [Event]
sortEvents es =  sortBy (comparing startTime) es


formatEvent :: TimeZone -> Event -> String
formatEvent tz e = "\n" ++ name e ++ "\n  " ++ "Starts At: "
                   ++ formatTime defaultTimeLocale "%c"  (utcToLocalTime tz (startTime e)) ++ "\n  "
                   ++ "End Time: " ++ formatTime defaultTimeLocale "%c"  (utcToLocalTime tz (endTime e)) ++ "\n  " 
                   ++  detail e ++ "\n" ++ "-------------------------" ++ "\n"

eventsToString :: TimeZone -> [Event] -> String
eventsToString tz es | null es = "No Events For You."
                     | otherwise = concatMap (formatEvent tz) (sortEvents es)

displayEvents :: IO ()
displayEvents = do es <- getCurrentEvents
                   tz <- getCurrentTimeZone
                   putStrLn (eventsToString tz es)


{- read in minutes, or hours  then convert to NominalDiffTime
then iterate through list and if the element startTime - current Time (diffUtcTime) is
equal to the nominal diff time then put in seperate list and display list -}












