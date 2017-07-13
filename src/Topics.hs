module Topics where

import Domain
import Lib.Prelude hiding (all)
import Data.Time

all :: [Topic]
all =
  [ Topic 1 "Topic title 1" "Topic description 1" (UTCTime (fromGregorian 2016 1 1) 1)
  , Topic 2 "Topic title 2" "Topic description 2" (UTCTime (fromGregorian 2017 1 1) 2)
  ]

lookup :: TopicId -> Maybe Topic
lookup id = find ((== id) . getTopicId) all

upsert :: Topic -> IO ()
upsert _ = return () -- TODO
