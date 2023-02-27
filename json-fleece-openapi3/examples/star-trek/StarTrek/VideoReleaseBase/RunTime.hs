{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.VideoReleaseBase.RunTime
  ( RunTime(..)
  , runTimeSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype RunTime = RunTime Integer
  deriving (Show, Eq)

runTimeSchema :: FC.Fleece schema => schema RunTime
runTimeSchema =
  FC.coerceSchema FC.integer