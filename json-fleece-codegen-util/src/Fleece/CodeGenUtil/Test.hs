{-# LANGUAGE RankNTypes #-}

module Fleece.CodeGenUtil.Test
  ( assertGoldenMatchesGenerated
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Enc

import qualified Fleece.CodeGenUtil as CGU

assertGoldenMatchesGenerated ::
  Monad m =>
  (forall a. (Eq a, Show a) => a -> a -> m ()) ->
  [(FilePath, BS8.ByteString)] ->
  CGU.Modules ->
  m ()
assertGoldenMatchesGenerated assertEquals goldenCodeFiles actualFiles = do
  let
    expectedFileNames =
      Set.filter
        (\path -> List.isSuffixOf ".hs" path)
        (Set.fromList (map fst goldenCodeFiles))

    actualFileNames =
      Set.fromList (map fst actualFiles)

    assertFileMatch (name, haskellCode) = do
      -- split the lines here for the sake of the diff in the hedgehog
      -- output
      assertEquals
        (Just (BS8.lines (Enc.encodeUtf8 (CGU.renderText haskellCode))))
        (fmap BS8.lines (lookup name goldenCodeFiles))

  assertEquals actualFileNames expectedFileNames
  traverse_ assertFileMatch actualFiles
