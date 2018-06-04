{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Deadline (Deadline(..)
                , upcomingDeadlines
                , deadlinesToLines
                , getCurrentDeadlines
                , addDeadline
                , addMinutes
                , displayAllDeadlines
                , sortDeadlines
                ) where

import           Data.Aeson
import           Data.List            (sort,sortBy)
import           Data.Ord             (comparing)
import           Data.Time.Clock      (NominalDiffTime, UTCTime, addUTCTime,
                                       getCurrentTime)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime
import           GHC.Generics
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T

data Deadline = Deadline { title       :: T.Text
                         , description :: T.Text
                         , dueDate     :: UTCTime
                         } deriving (Eq, Generic, ToJSON, FromJSON)

{- How to use a Deadline
... Assuming you're using `stack ghci`
import Data.Time
:set -XOverloadedStrings
t <- getCurrentTime
d1 = Deadline "First deadline" "It's the first deadline ever" (addMinutes 60 t)
d2 = Deadline "Second deadline" "It's the second deadline ever" (addMinutes 180 t)
d3 = Deadline "Third deadline" "Why so many deadlines?" (addMinutes 600 t)
l1 <- addDeadline d1 ([] :: [Deadline])
l2 <- addDeadline d2 l1
l3 <- addDeadline d3 l2
-- What are all of my upcoming tasks?
putStrLn (deadlinesToLines l3)
-- What do I need to do next?
upcomingDeadlines l3 >>= putStrLn . deadlinesToLines
-- Now we test writing to a file
(Just l') <- getCurrentDeadlines
-- I should be true!
l' == l3
-}

instance Ord Deadline where
  compare (Deadline _ _ aDate) (Deadline _ _ bDate) = compare aDate bDate

instance Show Deadline where
  show d = T.unpack (title d) ++ "\n  "
           ++ formatTime defaultTimeLocale "%c" time ++ "\n  "
           ++ T.unpack (description d) ++ "\n\n"
    where time = utcToLocalTime (unsafePerformIO getCurrentTimeZone) (dueDate d)

-- | The location of deadline files. Assumed to be in the current directory.
deadlineFile :: String
deadlineFile = "Deadlines"

-- | Reads deadlines from deadlineFile and returns a list of deadlines.
-- If data can't be found, then an empty list is returned.
getCurrentDeadlines :: IO [Deadline]
getCurrentDeadlines = decodeFileStrict deadlineFile >>= (\x -> case x of
                        (Just ds) -> return $ sort ds
                        Nothing   -> return [])

-- | Adds a deadline to a list of deadlines and immediately writes the deadline
-- to the deadlineFile
addDeadline :: Deadline -> [Deadline] -> IO [Deadline]
addDeadline d ds = do encodeFile deadlineFile ds' --writeDeadlinesToFile deadlineFile ds'
                      return ds'
  where ds' = d:ds

-- | Filters a list of deadlines to only contain deadlines with a 'dueDate' before
-- a specific 'UTCTime'.
deadlinesBefore :: UTCTime    -- ^ Max dueDate for deadlines
                -> [Deadline] -- ^ A list of deadlines
                -> [Deadline] -- ^ All deadlines occurring in the next 5 hours
deadlinesBefore t = Prelude.filter ((< t) . dueDate)

-- | Finds all deadlines coming due in the next 5 hours.
upcomingDeadlines :: [Deadline] -> IO [Deadline]
upcomingDeadlines xs = currentTimePlusMinutes 300
  >>= (\time -> return (deadlinesBefore time xs))

-- | Adds some number of minutes to a 'UTCTime'
addMinutes :: NominalDiffTime -> UTCTime -> UTCTime
addMinutes minutes = addUTCTime (minutes * 60)

-- | Adds some number of minutes to the current time.
currentTimePlusMinutes :: NominalDiffTime -> IO UTCTime
currentTimePlusMinutes minutes = addUTCTime (minutes * 60) <$> getCurrentTime

-- | Displays all deadlines in the 'deadlineFile'
displayAllDeadlines :: IO()
displayAllDeadlines = do ds <- getCurrentDeadlines
                         putStrLn (deadlinesToLines ds)

-- | Converts a list of deadlines into a well-formatted string for display
-- purposes. If no deadlines are available, displays an appropriate message.
deadlinesToLines :: [Deadline] -- ^ A list of deadlines
                 -> String     -- ^ A formatted string of deadlines
deadlinesToLines ds | null ds   = "There are no deadlines to display.\n\n"
                    | otherwise = concatMap show (sort ds)

sortDeadlines :: [Deadline] -> [Deadline]
sortDeadlines ds =  sortBy (comparing dueDate) ds
