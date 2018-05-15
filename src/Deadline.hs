{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Deadline where

import Data.ByteString as B
import Data.ByteString.Lazy as L
import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Aeson
import GHC.Generics

data Deadline = Deadline { title :: Text
                         , description :: Text
                         , dueDate :: UTCTime
                         } deriving (Show, Eq, Generic, ToJSON, FromJSON)

{- How to use a Deadline

t <- getCurrentTime
d = Deadline ("First deadline"::Text) ("It's the first deadline ever"::Text) t
l = [d,d,d]

e = Data.Aeson.encode d
(Just d') = (Data.Aeson.decode e :: Maybe Deadline)
d == d'

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

readFromFile :: FilePath -> IO (Maybe [Deadline])
readFromFile fp = do
    f <- L.readFile fp
    return (decode f)


writeToFile :: FilePath -> [Deadline] -> IO ()
writeToFile fp = L.writeFile fp . encode



upcomingDeadlines :: DiffTime   -- ^ Duration for alert
                  -> [Deadline] -- ^ A list of deadlines
                  -> [Deadline] -- ^ All deadlines occurring in the next 5 hours
upcomingDeadlines = error "not implemented"

overlaps :: Deadline -> Deadline -> Bool
overlaps = error "not implemented"
