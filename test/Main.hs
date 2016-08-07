module Main
    ( main
    ) where

import Data.RoundRobin

import Control.Monad
import Data.List.NonEmpty as NE
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic


main :: IO ()
main = defaultMain $ testGroup "Data.RoundRobin"
  [ selectTests
  ]


selectTests :: TestTree
selectTests = testGroup "select"
  [ testProperty "can always produce an element" $ \(NonEmpty (x:xs)) -> monadicIO $ do
      let ne = x :| xs :: NonEmpty ()
      rr <- run (newRoundRobin ne)
      let len = NE.length ne
      results <- run (replicateM (NE.length ne + 1) (select rr))
      assert (all (== ()) results)
  , testProperty "returns items in the order supplied" $ \(NonEmpty orig@(x:xs)) -> monadicIO $ do
     let ne = x :| xs :: NonEmpty ()
     rr <- run (newRoundRobin ne)
     let len = NE.length ne
     results <- run (replicateM (NE.length ne) (select rr))
     assert (results == orig)
  ]

