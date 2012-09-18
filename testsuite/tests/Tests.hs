module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Journey.Tests.MCTTree as T

main :: IO ()
main = defaultMain tests

tests = [ testGroup "QuickCheck MCTTree" [
            testProperty "lookup" T.prop_lookup
          ]
        ]

