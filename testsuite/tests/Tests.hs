module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Journey.TestDecisionTree as DT

main :: IO ()
main = defaultMain tests

tests = [ testGroup "QuickCheck Journey.DecisionTree" [
            testProperty "retrieve" DT.prop_retrieve
          ]
        ]

