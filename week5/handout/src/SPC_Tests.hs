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
      [
        -- Write test for pingSPC
        testCase "pingSPC returns 2" $ do
          spc <- startSPC
          result <- pingSPC spc
          result @?= 2,
        -- 
        testCase "Multiple pings" $ do
          spc <- startSPC
          results <- mapM (const $ pingSPC spc) [1 .. 5]
          results @?= replicate 5 2,
        --
        testCase "ping - exp. from exercise" $ do
          spc <- startSPC
          x <- pingSPC spc
          x @?= 2
      ]
