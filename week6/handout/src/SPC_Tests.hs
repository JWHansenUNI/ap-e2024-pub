module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [ testCase "adding job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          r <- jobStatus spc j
          r @?= Just JobPending
      ]
