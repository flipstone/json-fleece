{-# LANGUAGE DataKinds #-}

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

mkJSONText :: T.Text -> AnyJSON
mkJSONText = AnyJSON . unify

getJSONText :: AnyJSON -> Maybe T.Text
getJSONText =
  let
    noText = const Nothing
  in
    handleAnyJSON Just noText noText noText noText noText

mkJSONBool :: Bool -> AnyJSON
mkJSONBool = AnyJSON . unify

getJSONBool :: AnyJSON -> Maybe Bool
getJSONBool =
  let
    noBool = const Nothing
  in
    handleAnyJSON noBool Just noBool noBool noBool noBool

mkJSONNumber :: Scientific -> AnyJSON
mkJSONNumber = AnyJSON . unify

getJSONNumber :: AnyJSON -> Maybe Scientific
getJSONNumber =
  let
    noNumber = const Nothing
  in
    handleAnyJSON noNumber noNumber Just noNumber noNumber noNumber

mkJSONArray :: [AnyJSON] -> AnyJSON
mkJSONArray = AnyJSON . unify

getJSONArray :: AnyJSON -> Maybe [AnyJSON]
getJSONArray =
  let
    noArray = const Nothing
  in
    handleAnyJSON noArray noArray noArray Just noArray noArray

mkJSONObject :: Map.Map T.Text AnyJSON -> AnyJSON
mkJSONObject = AnyJSON . unify

getJSONObject :: AnyJSON -> Maybe (Map.Map T.Text AnyJSON)
getJSONObject =
  let
    noObject = const Nothing
  in
    handleAnyJSON noObject noObject noObject noObject Just noObject

mkJSONNull :: AnyJSON
mkJSONNull = AnyJSON (unify Null)

getJSONNull :: AnyJSON -> Maybe Null
getJSONNull =
  let
    noNull = const Nothing
  in
    handleAnyJSON noNull noNull noNull noNull noNull Just

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

anyJSON :: Fleece schema => schema AnyJSON
anyJSON =
  transform (\(AnyJSON u) -> u) AnyJSON $
    unionNamed (unqualifiedName "AnyJSON") $
      unionMember text
        #| unionMember boolean
        #| unionMember number
        #| unionMember anyArray
        #| unionMember anyObject
        #| unionMember Fleece.Core.Class.null

anyArray :: Fleece schema => schema [AnyJSON]
anyArray =
  list anyJSON

anyObject :: Fleece schema => schema (Map.Map T.Text AnyJSON)
anyObject =
  objectNamed (unqualifiedName "AnyJSON object") $
    constructor id
      #* additionalFields id anyJSON
