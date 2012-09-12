module Main where

import Test.Framework (defaultMain, testGroup, Test)

import Journey.TestDecisionTree

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
        testGroup "QuickCheck Journey.DecisionTree" []
        ]
