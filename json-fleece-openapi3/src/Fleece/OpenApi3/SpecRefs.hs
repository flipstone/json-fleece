{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3.SpecRefs
  ( SpecDialect (..)
  , SchemaAliases
  , schemaAliasNames
  , immediateAliasTarget
  , finalAliasTarget
  , SpecRefs (..)
  , specSchemaRefs
  ) where

import Data.Either (partitionEithers)
import qualified Data.Graph as Graph
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

finalAliasTarget :: T.Text -> SchemaAliases -> Maybe T.Text
finalAliasTarget name aliases =
  let
    go current =
      case immediateAliasTarget current aliases of
        Nothing -> current
        Just target -> go target
  in
    fmap go (immediateAliasTarget name aliases)

data SpecRefs = SpecRefs
  { specRefAliases :: SchemaAliases
  , specRefDeferredSchemaErrors :: Map.Map T.Text T.Text
  , specRefDeferredPathErrors :: Map.Map T.Text T.Text
  }

specSchemaRefs ::
  SpecDialect ->
  FC.AnyJSON ->
  SpecRefs
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
          Just (Left (name, NonStringRef name))
        RefText ref ->
          Just $ case T.stripPrefix refPrefix ref of
            Nothing ->
              Left (name, MalformedRef name ref)
            Just target ->
              if Map.member target definitions
                then Right (name, target)
                else Left (name, AliasTargetMissing name target)

    (aliasErrors, entries) =
      partitionEithers
        . mapMaybe aliasEntry
        . Map.toList
        $ definitions

    candidateTargets =
      Map.fromList entries

    cycles =
      aliasCycles candidateTargets

    errorForEachMember chain =
      fmap (\name -> (name, AliasCycle chain)) (NEL.toList chain)

    cycleErrorsByName =
      foldMap errorForEachMember cycles

    aliasTargets =
      Map.withoutKeys candidateTargets (Set.fromList (foldMap NEL.toList cycles))
  in
    SpecRefs
      { specRefAliases = SchemaAliases aliasTargets
      , specRefDeferredSchemaErrors =
          fmap renderSpecRefError (Map.fromList (aliasErrors <> cycleErrorsByName))
      , specRefDeferredPathErrors =
          fmap renderSpecRefError (pathItemRefErrors value (dialectPathItemsPath dialect))
      }

aliasCycles :: Map.Map T.Text T.Text -> [NonEmpty T.Text]
aliasCycles aliasTargets =
  let
    toNode (name, target) =
      (name, name, [target])

    cycleChain members =
      let
        start =
          NEL.head members

        follow name =
          Map.findWithDefault name name aliasTargets
      in
        start :| take (NEL.length members) (iterate follow (follow start))

    componentCycle component =
      case component of
        Graph.AcyclicSCC _name ->
          Nothing
        Graph.CyclicSCC names ->
          fmap cycleChain (NEL.nonEmpty names)
  in
    mapMaybe componentCycle
      . Graph.stronglyConnComp
      . fmap toNode
      . Map.toList
      $ aliasTargets

pathItemRefErrors ::
  FC.AnyJSON ->
  DocumentPath ->
  Map.Map T.Text SpecRefError
pathItemRefErrors value path =
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
            Just (key, NonStringRefAtUnsupportedPosition (position key))
          RefText ref ->
            Just (key, RefAtUnsupportedPosition (position key) ref)
  in
    Map.fromList (mapMaybe toError (Map.toList (objectAtPath segments value)))

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

dialectPathItemsPath :: SpecDialect -> DocumentPath
dialectPathItemsPath dialect =
  case dialect of
    OpenApi3Dialect -> DocumentPath ("paths" :| [])
    Swagger2Dialect -> DocumentPath ("paths" :| [])

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
