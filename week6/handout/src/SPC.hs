module SPC
  ( -- * SPC startup
    SPC,
    Job(..),
    JobDoneReason(..),
    JobStatus(..),
    startSPC,
    jobAdd,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void, Functor)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)
import Control.Applicative (Applicative)
import GenServer (requestReply)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

data Job = Job { jobAction :: IO (), jobMaxSeconds :: Int}
newtype JobId = JobId Int deriving (Eq, Ord, Show)

data JobDoneReason = Done
  | DoneTimeOut
  | DoneCancelled
  | DoneCrashed
  deriving (Eq, Ord, Show)

data JobStatus = JobDone JobDoneReason
  | JobRunning
  | JobPending
  deriving (Eq, Ord, Show)

data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobCounter :: JobId
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

put :: SPCState -> SPCM()
put state = SPCM $ \_ -> pure ((), state)

io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg = MsgJobAdd Job (ReplyChan JobId)
  | MsgJobStatus JobId (ReplyChan JobStatus)

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)



jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job = requestReply c $ MsgJobAdd job

jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case lookup jobid $ spcJobsPending state of
        Just _ -> Just JobPending
        _ -> Nothing

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = []
          }
  server <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC server
