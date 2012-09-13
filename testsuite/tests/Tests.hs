module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Journey.Tests.MinIntTree as T

main :: IO ()
main = defaultMain tests

tests = [ testGroup "QuickCheck MinIntTree" [
            testProperty "retrieve" T.prop_retrieve
          ]
        ]

