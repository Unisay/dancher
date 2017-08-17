module Topic.Repo where

import Topic.Domain
import Lib.Prelude hiding (all)
import Data.Pool
import Database.PostgreSQL.Simple
import qualified Database as Db (Connections)

all :: Db.Connections -> IO [(TopicId, Topic)]
all db = withResource db $ \c -> query_ c "select id, content from topic"

lookup :: Db.Connections -> TopicId -> IO (Maybe Topic)
lookup db id = withResource db $ \c -> do
  rs <- query c "select content from topic where id = ?" (Only id)
  return $ head (fmap fromOnly rs)

upsert :: Db.Connections -> TopicId -> Topic -> IO ()
upsert db id topic = withResource db $ \c ->
  void $ execute c "insert into topic (id, content) values (?, ?)" (id, topic)

insert :: Db.Connections -> Topic -> IO TopicId
insert db topic = withResource db $ \c ->
  execute c "insert into topic (content) values (?)" (Only topic)

delete :: Db.Connections -> TopicId -> IO Bool
delete db id = withResource db $ \c ->
  (== id) <$> execute c "delete from topic where id = ?" (Only id)
