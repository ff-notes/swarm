module Database (loadAllMessages, newMessage) where

import           Control.Monad.IO.Class (liftIO)
import           Data.List (sortOn)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           Data.Traversable (for)
import           RON.Data.ORSet.Experimental (ORSet)
import qualified RON.Data.ORSet.Experimental as ORSet
import           RON.Error (MonadE)
import           RON.Event (ReplicaClock)
import           RON.Store (MonadStore, newObject, openNamedObject, readObject)
import           RON.Store.FS (Handle, runStore)
import           RON.Types (Atom (AString), ObjectRef)

import           Types (Message, postTime)

loadAllMessages :: Handle -> IO [Message]
loadAllMessages db =
  runStore db $ do
    gMessages   <- openMessages
    mMessageSet <- readObject gMessages
    case mMessageSet of
      Nothing -> do
        liftIO $ putStrLn "!!! messages collection doesn't exist !!!"
        pure []
      Just messageSet -> do
        messageRefs <- ORSet.toList messageSet
        sortOn postTime . catMaybes <$> for messageRefs readObject

openMessages ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  m (ObjectRef (ORSet (ObjectRef Message)))
openMessages = openNamedObject "messages"

newMessage ::
  (MonadE m, MonadStore m, ReplicaClock m) =>
  Text -> Text -> m (ObjectRef Message)
newMessage username text = do
  gMessages <- openMessages
  msgRef <- newObject @Message
  ORSet.add_ msgRef ("username", [AString username])
  ORSet.add_ msgRef ("text",     [AString text    ])
  ORSet.add_ gMessages msgRef
  pure msgRef
