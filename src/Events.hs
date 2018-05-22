{-#  LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Events where

import           Data.Aeson
import           Data.List            (sort)
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

e1 = Event "First Event" "Some details" (addMinutes 2000 t) (addMinutes 2030 t)
e2 = Event "Second Event" "Some other details" (addMinutes 30 t) (addMinutes 180 t)

es <- addEvent e1 ([] :: [Event])
es' <- addEvent e2 es

es'' <- getCurrentEvents

-- Are the lists the same?
es'' == es'

-}

eventFile :: String
eventFile = "Events"

-- read from event file
getCurrentEvents :: IO [Event]
getCurrentEvents = decodeFileStrict eventFile >>= (\x -> case x of
                        (Just ts) -> return ts
                        Nothing   -> return [])

-- write to the event file
addEvent :: Event -> [Event] -> IO [Event]
addEvent x es = do encodeFile eventFile es'
                   return es'
                where es'= (listOfEvents x es)

-- adds the new event to the list of events if it does not overlap with any existing events
listOfEvents :: Event -> [Event] -> [Event]
listOfEvents x es = if (existingEvent x es) then es else (x:es)


-- checks if the new event overlaps with any existing events in the list
existingEvent :: Event -> [Event] -> Bool
existingEvent x [] = False
existingEvent x (e:es) =  if (startTime x) < (endTime e) && (startTime e) < (endTime x)
                             then True
                          else (existingEvent x es)
