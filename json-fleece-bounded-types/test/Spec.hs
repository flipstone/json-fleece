{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import qualified Data.Text.Lazy.Encoding as LTE
import qualified Test.Tasty as Tasty
import Test.Tasty.Golden (goldenVsStringDiff)

import qualified Fleece.BoundedTypes as FBT
import qualified Fleece.Core as FC
import qualified Fleece.Markdown as FM

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "json-fleece-bounded-types"
      [ testGroup
      ]

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "Markdown Rendering"
    [ test_nonEmptySet
    , test_boundedText
    ]

test_nonEmptySet :: Tasty.TestTree
test_nonEmptySet =
  mkGoldenTest
    "NonEmptySet includes minItems bound in the rendered Markdown"
    "test/examples/non-empty-set.md"
    (FBT.nonEmptySet FC.AllowInputDuplicates FC.text)

test_boundedText :: Tasty.TestTree
test_boundedText =
  mkGoldenTest
    "BoundedText includes minLength and maxLength bounds in the rendered Markdown"
    "test/examples/bounded-text.md"
    (FBT.boundedText @5 @20)

mkGoldenTest ::
  Tasty.TestName ->
  FilePath ->
  FC.Schema FM.Markdown a ->
  Tasty.TestTree
mkGoldenTest testName goldenPath schema =
  goldenVsStringDiff
    testName
    (\ref new -> ["diff", "-u", ref, new])
    goldenPath
    (pure . LTE.encodeUtf8 . FM.renderMarkdown $ schema)
