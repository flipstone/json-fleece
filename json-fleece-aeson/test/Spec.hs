module Main
  ( main
  ) where

import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import HTTPTests (httpTests)
import JSONTests (jsonTests)

main :: IO ()
main =
  HHM.defaultMain $
    [ HH.checkParallel jsonTests
    , HH.checkSequential httpTests
    ]
