{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3.SpecRefs
  ( SpecDialect (..)
  , specDialectName
  , SchemaAliases
  , schemaAliasNames
  , immediateAliasTarget
  , finalAliasTarget
  , SpecRefError
  , renderSpecRefError
  , specSchemaRefs
  ) where

import Data.Either (partitionEithers)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Fleece.Core as FC

data SpecDialect
  = OpenApi3Dialect
  | Swagger2Dialect

specDialectName :: SpecDialect -> T.Text
specDialectName dialect =
  case dialect of
    OpenApi3Dialect -> "OpenAPI"
    Swagger2Dialect -> "Swagger"

{- | The schemas that are defined as only a reference to another schema. When parsing there is nowhere
to record such a reference, so it is recovered from the unparsed document instead.
-}
newtype SchemaAliases = SchemaAliases
  { schemaAliasTargets :: Map.Map T.Text T.Text
  }

data SpecRefError
  = MalformedRef T.Text T.Text
  | NonStringRef T.Text
  | AliasTargetMissing T.Text T.Text
  | AliasCycle (NonEmpty T.Text)
  | RefAtUnsupportedPosition T.Text T.Text
  | NonStringRefAtUnsupportedPosition T.Text

renderSpecRefError :: SpecRefError -> T.Text
renderSpecRefError err =
  case err of
    MalformedRef name ref ->
      "The schema "
        <> quoted name
        <> " is a $ref to "
        <> quoted ref
        <> ", which does not name a schema in this document."
        <> " Only references to schemas in the same document are supported."
    NonStringRef name ->
      "The schema "
        <> quoted name
        <> " has a $ref that is not a string."
    AliasTargetMissing name target ->
      "The schema "
        <> quoted name
        <> " is a $ref to "
        <> quoted target
        <> ", which is not defined in this document."
    AliasCycle chain ->
      "The schemas "
        <> T.intercalate " -> " (fmap quoted (NEL.toList chain))
        <> " form a cycle of $ref aliases."
    NonStringRefAtUnsupportedPosition position ->
      "Found a $ref that is not a string at "
        <> position
        <> ". Code cannot be generated for a $ref in that position because the"
        <> " parsed representation of the document has nowhere to record it."
        <> " Inline the referenced definition at that position instead."
    RefAtUnsupportedPosition position ref ->
      "Found a $ref to "
        <> quoted ref
        <> " at "
        <> position
        <> ". Code cannot be generated for a $ref in that position because the"
        <> " parsed representation of the document has nowhere to record it."
        <> " Inline the referenced definition at that position instead."

quoted :: T.Text -> T.Text
quoted text =
  "\"" <> text <> "\""

schemaAliasNames :: SchemaAliases -> [T.Text]
schemaAliasNames =
  Map.keys . schemaAliasTargets

immediateAliasTarget :: T.Text -> SchemaAliases -> Maybe T.Text
immediateAliasTarget name =
  Map.lookup name . schemaAliasTargets

{- | The first schema in an alias chain that is not itself an alias. Terminates because cycles are
rejected when the aliases are collected.
-}
finalAliasTarget :: T.Text -> SchemaAliases -> Maybe T.Text
finalAliasTarget name aliases =
  let
    go current =
      case immediateAliasTarget current aliases of
        Nothing -> current
        Just target -> go target
  in
    fmap go (immediateAliasTarget name aliases)

{- | Collects the schema aliases in a document, rejecting references that code generation cannot
honour. Both checks happen here so that neither can be skipped nor run out of order.
-}
specSchemaRefs ::
  SpecDialect ->
  FC.AnyJSON ->
  Either (NonEmpty SpecRefError) SchemaAliases
specSchemaRefs dialect value =
  let
    definitionsPath =
      dialectDefinitionsPath dialect

    definitions =
      objectAtPath (documentPathSegments definitionsPath) value

    refPrefix =
      localRefPrefix definitionsPath

    aliasEntry (name, memberValue) =
      case memberRef memberValue of
        NoRef ->
          Nothing
        RefNotText ->
          Just (Left (NonStringRef name))
        RefText ref ->
          Just $ case T.stripPrefix refPrefix ref of
            Nothing ->
              Left (MalformedRef name ref)
            Just target ->
              if Map.member target definitions
                then Right (name, target)
                else Left (AliasTargetMissing name target)

    (aliasErrors, entries) =
      partitionEithers
        . mapMaybe aliasEntry
        . Map.toList
        $ definitions

    aliasTargets =
      Map.fromList entries

    unsupportedErrors =
      foldMap
        (unsupportedRefsAt value)
        (dialectUnsupportedPaths dialect)
  in
    case unsupportedErrors <> aliasErrors <> cycleErrors aliasTargets of
      [] -> Right (SchemaAliases aliasTargets)
      firstError : rest -> Left (firstError :| rest)

{- | Reports each cycle once, naming only the schemas the cycle runs through. A name that merely leads
into a cycle is not part of it, and a name already walked is not walked again.
-}
cycleErrors :: Map.Map T.Text T.Text -> [SpecRefError]
cycleErrors aliasTargets =
  let
    walk seen walked name =
      case Map.lookup name aliasTargets of
        Nothing ->
          (Set.union seen (Set.fromList walked), Nothing)
        Just target ->
          if target `elem` walked
            then
              let
                cycleNames =
                  dropWhile (/= target) (reverse walked) <> [target]
              in
                ( Set.union seen (Set.fromList walked)
                , fmap AliasCycle (NEL.nonEmpty cycleNames)
                )
            else
              if Set.member target seen
                then (Set.union seen (Set.fromList walked), Nothing)
                else walk seen (target : walked) target

    step (seen, errs) start =
      if Set.member start seen
        then (seen, errs)
        else case walk seen [start] start of
          (nextSeen, Nothing) -> (nextSeen, errs)
          (nextSeen, Just err) -> (nextSeen, err : errs)
  in
    reverse . snd $ Foldable.foldl' step (Set.empty, []) (Map.keys aliasTargets)

unsupportedRefsAt ::
  FC.AnyJSON ->
  DocumentPath ->
  [SpecRefError]
unsupportedRefsAt value path =
  let
    segments =
      documentPathSegments path

    position key =
      T.intercalate "." (segments <> [key])

    toError (key, memberValue) =
      if isExtensionKey key
        then Nothing
        else case memberRef memberValue of
          NoRef ->
            Nothing
          RefNotText ->
            Just (NonStringRefAtUnsupportedPosition (position key))
          RefText ref ->
            Just (RefAtUnsupportedPosition (position key) ref)
  in
    mapMaybe toError (Map.toList (objectAtPath segments value))

{- | Specification extensions may sit alongside the members of the maps this
guards, and carry no meaning for code generation.
-}
isExtensionKey :: T.Text -> Bool
isExtensionKey =
  T.isPrefixOf "x-"

data MemberRef
  = NoRef
  | RefText T.Text
  | RefNotText

memberRef :: FC.AnyJSON -> MemberRef
memberRef value =
  case FC.getJSONObject value >>= Map.lookup "$ref" of
    Nothing ->
      NoRef
    Just refValue ->
      case FC.getJSONText refValue of
        Just ref -> RefText ref
        Nothing -> RefNotText

newtype DocumentPath
  = DocumentPath (NonEmpty T.Text)

documentPathSegments :: DocumentPath -> [T.Text]
documentPathSegments (DocumentPath segments) =
  NEL.toList segments

localRefPrefix :: DocumentPath -> T.Text
localRefPrefix path =
  "#/" <> T.intercalate "/" (documentPathSegments path) <> "/"

dialectDefinitionsPath :: SpecDialect -> DocumentPath
dialectDefinitionsPath dialect =
  case dialect of
    OpenApi3Dialect -> DocumentPath ("components" :| ["schemas"])
    Swagger2Dialect -> DocumentPath ("definitions" :| [])

{- | Maps that code generation reads but whose parsed representation has
nowhere to record a reference. A reference under @components@ is left to the
document parser, which rejects it because those objects have required fields.
-}
dialectUnsupportedPaths :: SpecDialect -> [DocumentPath]
dialectUnsupportedPaths dialect =
  case dialect of
    OpenApi3Dialect -> [DocumentPath ("paths" :| [])]
    Swagger2Dialect -> [DocumentPath ("paths" :| [])]

objectAtPath :: [T.Text] -> FC.AnyJSON -> Map.Map T.Text FC.AnyJSON
objectAtPath path value =
  case FC.getJSONObject value of
    Nothing ->
      Map.empty
    Just obj ->
      case path of
        [] ->
          obj
        key : rest ->
          case Map.lookup key obj of
            Just child -> objectAtPath rest child
            Nothing -> Map.empty
