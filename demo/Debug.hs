import Control.Concurrent
import Control.Monad
import Debug.Trace
import qualified System.FSNotify as FSNotify

main :: IO ()
main = do
  debug
  forever $ threadDelay 100

debug :: IO ()
debug = do
  fsWatchManager <- FSNotify.startManager
  _ <-
    FSNotify.watchTree
      fsWatchManager
      "/Users/cblp/dev/ron"
      (const True)
      traceShowM
  pure ()
