module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Journey.Tests.MCTTree as T

main :: IO ()
main = defaultMain tests

tests = [ testGroup "QuickCheck MCTTree" [
            testProperty "lookup1" T.prop_lookup1
          , testProperty "lookup2" T.prop_lookup2
          , testProperty "lookup3" T.prop_lookup3
          , testProperty "lookup4" T.prop_lookup4
          , testProperty "lookup5" T.prop_lookup5
        ] ]

