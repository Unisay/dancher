module Topics where

import Domain
import Lib.Prelude hiding (all)
import Control.Monad.Trans.Maybe
import qualified Data.Map as M

type TopicRepo = MVar (Map TopicId Topic)

newTopicRepo :: IO TopicRepo
newTopicRepo = newMVar $ M.fromList [ (1, Topic 1 "Topic title 1" "Topic description 1")
                                    , (2, Topic 2 "Topic title 2" "Topic description 2")
                                    ]

generateId :: TopicRepo -> IO TopicId
generateId _ = return 3 -- TODO

lookup :: TopicRepo -> TopicId -> MaybeT IO Topic
lookup repo id = MaybeT $ do
  m <- readMVar repo
  return $ M.lookup id m

upsert :: TopicRepo -> Topic -> IO ()
upsert repo topic = do
  m <- takeMVar repo
  putMVar repo $! M.insert (getTopicId topic) topic m

insert :: TopicRepo -> Topic -> IO () -- TODO: error on duplicate id?
insert = upsert
