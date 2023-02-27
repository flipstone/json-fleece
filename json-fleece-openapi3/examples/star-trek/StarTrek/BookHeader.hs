{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookHeader
  ( BookHeader(..)
  , bookHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.BookHeader.Title (Title, titleSchema)
import StarTrek.BookHeader.Uid (Uid, uidSchema)

data BookHeader = BookHeader
  { uid :: Uid -- ^ Book unique ID
  , title :: Title -- ^ Book title
  }
  deriving (Eq, Show)

bookHeaderSchema :: FC.Fleece schema => schema BookHeader
bookHeaderSchema =
  FC.object $
    FC.constructor BookHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema