module Topic.Repo where

import Topic.Domain
import Lib.Prelude hiding (all)
import Control.Monad.Trans.Maybe
import qualified Data.Map as M

type Repo = MVar (Map TopicId Topic)

newTopicRepo :: IO Repo
newTopicRepo = newMVar $ M.fromList [ (1, Topic "Topic title 1" "Topic description 1")
                                    , (2, Topic "Topic title 2" "Topic description 2")
                                    ]

generateId :: Repo -> IO TopicId
generateId repo = do
  m <- readMVar repo
  let sorted = sortBy (flip compare) (M.keys m)
  return $ fromMaybe 1 ((+1) <$> head sorted)

all :: Repo -> IO [(TopicId, Topic)]
all repo = do
  m <- readMVar repo
  return $ M.assocs m

lookup :: Repo -> TopicId -> MaybeT IO Topic
lookup repo id = MaybeT $ do
  m <- readMVar repo
  return $ M.lookup id m

upsert :: Repo -> TopicId -> Topic -> IO ()
upsert repo id topic = do
  m <- takeMVar repo
  putMVar repo $! M.insert id topic m

insert :: Repo -> TopicId -> Topic -> IO () -- TODO: error on duplicate id?
insert = upsert
