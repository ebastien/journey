module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Journey.Tests.Ssim (ssimTests)

main :: IO ()
main = defaultMain tests

tests = [ testGroup "Ssim" ssimTests ]
