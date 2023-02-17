module Fleece.Examples
  ( FooBar(..)
  , fooBarSchema
  ) where

import Data.Scientific (Scientific)
import qualified Data.Text as T

import Fleece.Core (Fleece, required, object, constructor, text, number, (#+))

data FooBar =
  FooBar
    { foo :: T.Text
    , bar :: Scientific
    } deriving (Eq, Show)

fooBarSchema :: Fleece schema => schema FooBar
fooBarSchema =
  object $
    constructor FooBar
      #+ required "foo" foo text
      #+ required "bar" bar number
