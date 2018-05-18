{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Deadline (Deadline(..)
                , upcomingDeadlines
                , deadlinesToLines
                , getCurrentDeadlines
                , addDeadline
                , addMinutes
                , displayAllDeadlines
                ) where

import Data.ByteString.Lazy as L hiding (concatMap, putStrLn)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Aeson
import GHC.Generics
import Data.List (sort)

data Deadline = Deadline { title :: String
                         , description :: String
                         , dueDate :: UTCTime
                         } deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- How to use a Deadline

-- Assuming you're using `stack ghci`

import Data.Time
:set -XOverloadedStrings
t <- getCurrentTime
tz <- getCurrentTimeZone
d1 = Deadline "First deadline" "It's the first deadline ever" (addMinutes 60 t)
d2 = Deadline "Second deadline" "It's the second deadline ever" (addMinutes 180 t)
d3 = Deadline "Third deadline" "Why so many deadlines?" (addMinutes 600 t)
l1 <- addDeadline d1 ([] :: [Deadline])
l2 <- addDeadline d2 l1
l3 <- addDeadline d3 l2

-- What are all of my upcoming tasks?
putStrLn (deadlinesToLines tz l)

-- What do I need to do next?
upcomingDeadlines l >>= putStrLn . deadlinesToLines tz


-- Testing JSON encode/decode
e1 = Data.Aeson.encode d1
(Just d1') = (Data.Aeson.decode e1 :: Maybe Deadline)
d1 == d1'

-- Now we test writing to a file
writeToFile "Deadline" l
(Just l') = readFromFile "Deadline"

-- I should be true!
l' == l


-}

{-
instance FromJSON Deadline where
  parseJSON (Object v) =
    Deadline <$> v .: "title"
             <*> v .: "description"
             <*> v .: "dueDate"
             <*> v .: "duration"


instance ToJSON Deadline where
  toJSON (Deadline title description dueDate duration) =
    object [ "title"       .= title
           , "description" .= description
           , "dueDate"     .= dueDate
           , "duration"    .= duration
           ]
-}


instance Ord Deadline where
  compare (Deadline _ _ aDate) (Deadline _ _ bDate) = compare aDate bDate

deadlineFile :: String
deadlineFile = "Deadlines"

getCurrentDeadlines :: IO [Deadline]
getCurrentDeadlines = readDeadlinesFromFile deadlineFile >>= (\x -> case x of
                        (Just ds)  -> return ds
                        Nothing  -> return [])

addDeadline :: Deadline -> [Deadline] -> IO [Deadline]
addDeadline d ds = do writeDeadlinesToFile deadlineFile ds'
                      return ds'
  where ds' = d:ds


readDeadlinesFromFile :: FilePath -> IO (Maybe [Deadline])
readDeadlinesFromFile fp = do
    f <- L.readFile fp
    return (decode f)

writeDeadlinesToFile :: FilePath -> [Deadline] -> IO ()
writeDeadlinesToFile fp = L.writeFile fp . encode

deadlinesBefore :: UTCTime
                -> [Deadline] -- ^ A list of deadlines
                -> [Deadline] -- ^ All deadlines occurring in the next 5 hours
deadlinesBefore t = Prelude.filter ((< t) . dueDate)

upcomingDeadlines :: [Deadline] -> IO [Deadline]
upcomingDeadlines xs = currentTimePlusMinutes 300
  >>= (\time -> return (deadlinesBefore time xs))

addMinutes :: NominalDiffTime -> UTCTime -> UTCTime
addMinutes minutes = addUTCTime (minutes * 60)

currentTimePlusMinutes :: NominalDiffTime -> IO UTCTime
currentTimePlusMinutes minutes = addUTCTime (minutes * 60) <$> getCurrentTime

displayAllDeadlines :: IO()
displayAllDeadlines = do ds <- getCurrentDeadlines
                         tz <- getCurrentTimeZone
                         putStrLn (deadlinesToLines tz ds)

deadlinesToLines :: TimeZone -> [Deadline] -> String
deadlinesToLines tz ds = concatMap (deadlineToLine tz) (sort ds)

deadlineToLine :: TimeZone -> Deadline -> String
deadlineToLine tz d = title d ++ "\n  "
                   ++ formatTime defaultTimeLocale "%c" time ++ "\n  "
                   ++ description d ++ "\n\n"
  where time = utcToLocalTime tz (dueDate d)
