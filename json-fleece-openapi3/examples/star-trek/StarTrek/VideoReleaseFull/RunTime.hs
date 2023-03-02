{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseFull.RunTime
  ( RunTime(..)
  , runTimeSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype RunTime = RunTime Integer
  deriving (Show, Eq)

runTimeSchema :: FC.Fleece schema => schema RunTime
runTimeSchema =
  FC.coerceSchema FC.integer