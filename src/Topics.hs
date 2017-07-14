module Topics where

import Domain
import Lib.Prelude hiding (all)

all :: [Topic]
all =
  [ Topic 1 "Topic title 1" "Topic description 1"
  , Topic 2 "Topic title 2" "Topic description 2"
  ]

generateId :: IO TopicId
generateId = return 3 -- TODO

lookup :: TopicId -> Maybe Topic
lookup id = find ((== id) . getTopicId) all

upsert :: Topic -> IO ()
upsert _ = return () -- TODO

insert :: Topic -> IO () -- TODO: error on duplicate id?
insert _ = return () -- TODO
