{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseBase.RunTime
  ( RunTime(..)
  , runTimeSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype RunTime = RunTime Integer
  deriving (Show, Eq)

runTimeSchema :: FC.Fleece t => FC.Schema t RunTime
runTimeSchema =
  FC.coerceSchema FC.integer