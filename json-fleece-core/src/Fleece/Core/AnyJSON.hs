{-# LANGUAGE DataKinds #-}

{- | Defines the 'AnyJSON' type for representing arbitrary JSON values and a
schema for encoding\/decoding them.
-}
module Fleece.Core.AnyJSON
  ( AnyJSON (AnyJSON)
  , mkJSONText
  , getJSONText
  , mkJSONBool
  , getJSONBool
  , mkJSONNumber
  , getJSONNumber
  , mkJSONArray
  , getJSONArray
  , mkJSONObject
  , getJSONObject
  , mkJSONNull
  , getJSONNull
  , anyJSON
  , handleAnyJSON
  ) where

import qualified Data.Map as Map
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Shrubbery (Union, branch, branchBuild, branchEnd, dissectUnion, unify)

import Fleece.Core.Class
  ( Fleece
  , Null (Null)
  , Schema
  , additionalFields
  , boolean
  , constructor
  , null
  , number
  , objectNamed
  , text
  , transform
  , unionNamed
  , (#*)
  , (#|)
  )
import Fleece.Core.Name (unqualifiedName)
import Fleece.Core.Schemas (list, unionMember)

{- | Represents an arbitrary JSON value as a union of text, bool, number,
array, object, and null.
-}
newtype AnyJSON
  = AnyJSON
      ( Union
          '[ T.Text
           , Bool
           , Scientific
           , [AnyJSON]
           , Map.Map T.Text AnyJSON
           , Null
           ]
      )
  deriving (Show)

instance Eq AnyJSON where
  left == right =
    handleAnyJSON
      (\t -> getJSONText right == Just t)
      (\b -> getJSONBool right == Just b)
      (\n -> getJSONNumber right == Just n)
      (\a -> getJSONArray right == Just a)
      (\o -> getJSONObject right == Just o)
      (\u -> getJSONNull right == Just u)
      left

-- | Constructs an 'AnyJSON' value from 'T.Text'.
mkJSONText :: T.Text -> AnyJSON
mkJSONText = AnyJSON . unify

{- | Extracts a 'T.Text' from an 'AnyJSON' value, returning 'Nothing' if it is
not a text value.
-}
getJSONText :: AnyJSON -> Maybe T.Text
getJSONText =
  let
    noText = const Nothing
  in
    handleAnyJSON Just noText noText noText noText noText

-- | Constructs an 'AnyJSON' value from a 'Bool'.
mkJSONBool :: Bool -> AnyJSON
mkJSONBool = AnyJSON . unify

{- | Extracts a 'Bool' from an 'AnyJSON' value, returning 'Nothing' if it is
not a boolean value.
-}
getJSONBool :: AnyJSON -> Maybe Bool
getJSONBool =
  let
    noBool = const Nothing
  in
    handleAnyJSON noBool Just noBool noBool noBool noBool

-- | Constructs an 'AnyJSON' value from a 'Scientific' number.
mkJSONNumber :: Scientific -> AnyJSON
mkJSONNumber = AnyJSON . unify

{- | Extracts a 'Scientific' number from an 'AnyJSON' value, returning
'Nothing' if it is not a number value.
-}
getJSONNumber :: AnyJSON -> Maybe Scientific
getJSONNumber =
  let
    noNumber = const Nothing
  in
    handleAnyJSON noNumber noNumber Just noNumber noNumber noNumber

-- | Constructs an 'AnyJSON' value from a list of 'AnyJSON' values.
mkJSONArray :: [AnyJSON] -> AnyJSON
mkJSONArray = AnyJSON . unify

{- | Extracts a list of 'AnyJSON' values from an 'AnyJSON' value, returning
'Nothing' if it is not an array value.
-}
getJSONArray :: AnyJSON -> Maybe [AnyJSON]
getJSONArray =
  let
    noArray = const Nothing
  in
    handleAnyJSON noArray noArray noArray Just noArray noArray

{- | Constructs an 'AnyJSON' value from a 'Map.Map' of text keys to 'AnyJSON'
values.
-}
mkJSONObject :: Map.Map T.Text AnyJSON -> AnyJSON
mkJSONObject = AnyJSON . unify

{- | Extracts a 'Map.Map' from an 'AnyJSON' value, returning 'Nothing' if it
is not an object value.
-}
getJSONObject :: AnyJSON -> Maybe (Map.Map T.Text AnyJSON)
getJSONObject =
  let
    noObject = const Nothing
  in
    handleAnyJSON noObject noObject noObject noObject Just noObject

-- | Constructs an 'AnyJSON' value representing JSON null.
mkJSONNull :: AnyJSON
mkJSONNull = AnyJSON (unify Null)

{- | Extracts a 'Null' from an 'AnyJSON' value, returning 'Nothing' if it is
not null.
-}
getJSONNull :: AnyJSON -> Maybe Null
getJSONNull =
  let
    noNull = const Nothing
  in
    handleAnyJSON noNull noNull noNull noNull noNull Just

{- | Eliminates an 'AnyJSON' value by providing handler functions for each
possible JSON type: text, bool, number, array, object, and null.
-}
handleAnyJSON ::
  (T.Text -> a) ->
  (Bool -> a) ->
  (Scientific -> a) ->
  ([AnyJSON] -> a) ->
  (Map.Map T.Text AnyJSON -> a) ->
  (Null -> a) ->
  AnyJSON ->
  a
handleAnyJSON handleText handleBool handleNumber handleArray handleObject handleNull =
  let
    handler =
      dissectUnion
        . branchBuild
        . branch handleText
        . branch handleBool
        . branch handleNumber
        . branch handleArray
        . branch handleObject
        . branch handleNull
        $ branchEnd
  in
    \(AnyJSON u) -> handler u

-- | A Fleece schema for 'AnyJSON' values.
anyJSON :: Fleece t => Schema t AnyJSON
anyJSON =
  transform (\(AnyJSON u) -> u) AnyJSON $
    unionNamed (unqualifiedName "AnyJSON") $
      unionMember text
        #| unionMember boolean
        #| unionMember number
        #| unionMember anyArray
        #| unionMember anyObject
        #| unionMember Fleece.Core.Class.null

anyArray :: Fleece t => Schema t [AnyJSON]
anyArray =
  list anyJSON

anyObject :: Fleece t => Schema t (Map.Map T.Text AnyJSON)
anyObject =
  objectNamed (unqualifiedName "AnyJSON object") $
    constructor id
      #* additionalFields id anyJSON
