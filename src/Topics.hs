module Topics where

import Domain
import Lib.Prelude hiding (all)
import Control.Monad.Trans.Maybe
import qualified Data.Map as M

type Repo = MVar (Map TopicId Topic)

newTopicRepo :: IO Repo
newTopicRepo = newMVar $ M.fromList [ (1, Topic 1 "Topic title 1" "Topic description 1")
                                    , (2, Topic 2 "Topic title 2" "Topic description 2")
                                    ]

generateId :: Repo -> IO TopicId
generateId _ = return 3 -- TODO

all :: Repo -> IO [Topic]
all repo = do
  m <- readMVar repo
  return $ M.elems m

lookup :: Repo -> TopicId -> MaybeT IO Topic
lookup repo id = MaybeT $ do
  m <- readMVar repo
  return $ M.lookup id m

upsert :: Repo -> Topic -> IO ()
upsert repo topic = do
  m <- takeMVar repo
  putMVar repo $! M.insert (getTopicId topic) topic m

insert :: Repo -> Topic -> IO () -- TODO: error on duplicate id?
insert = upsert
