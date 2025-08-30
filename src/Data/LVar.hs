-- | @LVar@ is like @Control.Concurrent.STM.TMVar@ but with a capability for
-- listening to its changes.
module Data.LVar
  ( -- * Types
    LVar,

    -- * Creating a LVar
    new,
    empty,

    -- * Modifying a LVar
    get,
    set,
    modify,

    -- * Listening to a LVar
    listenNext,
  )
where

import Control.Concurrent.STM (STM, TMVar, TVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor (void)

-- A mutable variable (like @TMVar@), changes to which can be listened to from
-- multiple threads.
data LVar a
  = LVar
      (TMVar a)
      -- ^ A value that changes over time.
      (TVar (TMVar ()))
      -- ^ Gets filled when above gets written to.
      -- Invariant: value is always empty.

-- | Create a new @LVar@ with the given initial value
new :: forall a m. MonadIO m => a -> m (LVar a)
new val = liftIO $ do
  var <- STM.newTMVarIO val
  hole <- STM.newEmptyTMVarIO
  write <- STM.newTVarIO hole
  return $ LVar var write

-- | Like @new@, but there is no initial value. A @get@ will block until an
-- initial value is set using @set@ or @modify@
empty :: MonadIO m => m (LVar a)
empty = liftIO $ do
  var <- STM.newEmptyTMVarIO
  hole <- STM.newEmptyTMVarIO
  write <- STM.newTVarIO hole
  return $ LVar var write

-- | Get the value of the @LVar@
get :: MonadIO m => LVar a -> m a
get (LVar var _) = liftIO $ STM.atomically $ STM.readTMVar var

-- | Set the @LVar@ value; active listeners are automatically notifed.
set :: MonadIO m => LVar a -> a -> m ()
set (LVar var write) val = liftIO $ STM.atomically $ do
  STM.isEmptyTMVar var >>= \case
    True -> STM.putTMVar var val
    False -> void $ STM.swapTMVar var val
  notifyListeners write

-- | Modify the @LVar@ value; active listeners are automatically notified.
modify :: MonadIO m => LVar a -> (a -> a) -> m ()
modify (LVar var write) f = liftIO $ STM.atomically $ do
  curr <- STM.readTMVar var
  void $ STM.swapTMVar var (f curr)
  notifyListeners write

notifyListeners :: TVar (TMVar ()) -> STM ()
notifyListeners write = do
  new_hole <- STM.newEmptyTMVar
  old_hole <- STM.readTVar write
  STM.putTMVar old_hole ()
  STM.writeTVar write new_hole

-- | Listen for the next value update (since the last @listenNext@ or
-- @addListener@) and return the current value when that update occurs.
-- Returns immediately after the first change is detected, not after multiple rapid changes.
-- If multiple updates happen quickly, this returns the value after the first update.
listenNext :: MonadIO m => LVar a -> m a
listenNext (LVar var write) = liftIO $ do
  hole <- STM.readTVarIO write
  STM.atomically $ do
    () <- STM.readTMVar hole
    STM.readTMVar var
