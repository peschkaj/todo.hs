module Deadline
  ( Deadline
  ) where

import Data.ByteString
import Data.Thyme.LocalTime
import Data.Thyme.Clock

data Deadline = Deadline { title :: ByteString
                         , description :: ByteString
                         , dueDate :: LocalTime
                         , duration :: DiffTime
                         } deriving (Show, Eq)

instance Ord Deadline where
  compare (Deadline _ _ aDate aDuration) (Deadline _ _ bDate bDuration) =
    if dateComp == EQ
      then durComp
      else dateComp
    where
      dateComp = compare aDate bDate
      durComp = compare aDuration bDuration


readFromFile :: ByteString -> IO [Deadline]
readFromFile filename = error "not implemented"

writeToFile :: ByteString -> [Deadline] -> IO ()
writeToFile filename deadlines = error "not implemented"

upcomingDeadlines :: [Deadline] -- ^ A list of deadlines
                  -> [Deadline] -- ^ All deadlines occurring in the next 5 hours
upcomingDeadlines = error "not implemented"

overlaps :: Deadline -> Deadline -> Bool
overlaps = error "not implemented"
