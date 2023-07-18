{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Fleece.CodeGenUtil.Test
  ( assertGoldenMatchesGenerated
  , loadTestConfig
  ) where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as BS8
import qualified Data.FileEmbed as FileEmbed
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Dhall
import qualified System.Environment as Env
import System.FilePath (takeDirectory)

import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.CodeGenUtil.Config as Config

loadTestConfig ::
  (MIO.MonadIO m) =>
  (FilePath -> m BS8.ByteString) ->
  FilePath ->
  m Config.Config
loadTestConfig readTestFile dhallPath = do
  dhallBytes <- readTestFile dhallPath
  MIO.liftIO (Env.setEnv "CODEGEN_TEST_PRELUDE" (BS8.unpack configPrelude))
  mkConfig <- MIO.liftIO (Dhall.input Config.decoder (Enc.decodeUtf8 dhallBytes))

  let
    rootDir =
      takeDirectory dhallPath

    config =
      mkConfig (T.pack rootDir)

  pure config

configPrelude :: BS8.ByteString
configPrelude =
  -- last changed 2023-05-05 14:12:04
  $(FileEmbed.embedFile "codegen-prelude.dhall")

assertGoldenMatchesGenerated ::
  (Monad m) =>
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
