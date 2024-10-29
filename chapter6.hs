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

runThread :: IO ()
runThread = do
    t <- forkIO $ putStrLn "Hello there."
    print t

channelExample :: IO ()
channelExample = do
    c <- newChan
    _ <- forkIO $ do
        r <- readChan c
        putStrLn $ "Received message: " <> r
    writeChan c "Hello There"

channelLoopExample :: IO ()
channelLoopExample = do
  c <- newChan
  let threadLoop = do
        r <- readChan c
        putStrLn $ "Received message: " <> r
        threadLoop
  _ <- forkIO threadLoop
  writeChan c "The first"
  writeChan c "The second"
  writeChan c "The third"

data Msg = MsgInc Int (Chan Int)

threadLoop :: Chan Msg -> IO ()
threadLoop c = do
  msg <- readChan c
  case msg of
    MsgInc x from ->
      writeChan from (x + 1)
  threadLoop c

-- RPC (Remote Procedure Calls)
performRPC :: Chan Msg -> Int -> IO Int
performRPC c x = do
  from <- newChan
  writeChan c $ MsgInc x from
  readChan from

ex2 :: IO ()
ex2 = do
  c <- newChan
  _ <- forkIO $ threadLoop c
  print =<< performRPC c 0
  print =<< performRPC c 1

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
------------------------------------------------------------
------------------------------------------------------------


