{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Deadline (Deadline (..),) where

import Data.ByteString (ByteString)
import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Aeson
import GHC.Generics

data Deadline = Deadline { title :: Text
                         , description :: Text
                         , dueDate :: UTCTime
                         , duration :: DiffTime
                         } deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- How to use a Deadline

t <- getCurrentTime
d = Deadline ("First deadline"::Text) ("It's the first deadline ever"::Text) t (secondsToDiffTime 60 * 60)

e = Data.Aeson.encode d
(Just d') = (Data.Aeson.decode e :: Maybe Deadline)
d == d'

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
