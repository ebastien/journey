module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Journey.Tests.Ssim
import qualified Journey.Tests.Restriction

main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Ssim" Journey.Tests.Ssim.tests
  , testGroup "Restriction" Journey.Tests.Restriction.tests
  ]

