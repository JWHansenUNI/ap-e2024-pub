import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    killThread,
    newChan,
    readChan,
    threadDelay,
    writeChan,
  )
import Control.Monad (replicateM_)


data Server msg = Server ThreadId (Chan msg)

spawn :: (Chan a -> IO ()) -> IO (Server a)
spawn serverLoop = do
  input <- newChan
  tid <- forkIO $ serverLoop input
  return $ Server tid input

send :: Chan a -> a -> IO ()
send = writeChan

receive :: Chan a -> IO a
receive = readChan

sendTo :: Server a -> a -> IO ()
sendTo (Server _tid input) msg =
  send input msg

newtype ReplyChan a = ReplyChan (Chan a)

reply :: ReplyChan a -> a -> IO ()
reply (ReplyChan chan) x = send chan x

requestReply :: Server a -> (ReplyChan b -> a) -> IO b
requestReply serv con = do
  reply_chan <- newChan
  sendTo serv $ con $ ReplyChan reply_chan
  receive reply_chan

type InternalData = Int

data Msg = GetValue (ReplyChan Int)
         | Incr
         | Decr Int (ReplyChan Bool)

counterLoop :: InternalData -> Chan Msg -> IO ()
counterLoop state input = do
  msg <- receive input
  case msg of
    GetValue from -> do
      let (newState, res) = (state, state)
      reply from res
      counterLoop newState input
    Incr -> do
      let newState = state + 1
      counterLoop newState input
    Decr n from -> do
      let (newState, res) =
            case state of
              value | value > n -> (value - n, True)
              _                 -> (state, False)
      reply from res
      counterLoop newState input

type CounterServer = Server Msg

newCounter :: Int -> IO CounterServer
newCounter initial | initial >= 0 = spawn $ counterLoop initial
newCounter _                      = error "Initial value should be non-negative"

getValue :: CounterServer -> IO Int
getValue cnt = requestReply cnt GetValue

incr :: CounterServer -> IO ()
incr cnt = sendTo cnt Incr

decr :: CounterServer -> Int -> IO Bool
decr cnt n | n >= 0 = requestReply cnt $ Decr n
decr _ _            = error "Cannot decrement with negative amount"

main = do
  c <- newCounter 0
  incr c
  replicateM_ 5 $ incr c
  _ <- decr c 1
  v <- getValue c
  putStrLn $ "The counter should now be 5, and it is " ++ show v
