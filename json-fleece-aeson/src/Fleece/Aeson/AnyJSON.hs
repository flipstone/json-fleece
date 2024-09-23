module Fleece.Aeson.AnyJSON
  ( aesonValue
  , anyJSONToValue
  , valueToAnyJSON
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Fleece.Core as FC

aesonValue :: FC.Fleece schema => schema Aeson.Value
aesonValue =
  FC.validate (FC.transform valueToAnyJSON anyJSONToValue) FC.anyJSON

anyJSONToValue :: FC.AnyJSON -> Aeson.Value
anyJSONToValue =
  FC.handleAnyJSON
    Aeson.String
    Aeson.Bool
    Aeson.Number
    (\items -> Aeson.Array (Vector.fromList (fmap anyJSONToValue items)))
    (\propMap -> Aeson.Object (mapToKeyMap propMap))
    (\FC.Null -> Aeson.Null)

mapToKeyMap :: Map.Map T.Text FC.AnyJSON -> KeyMap.KeyMap Aeson.Value
mapToKeyMap =
  KeyMap.fromMap
    . Map.mapKeys Key.fromText
    . fmap anyJSONToValue

valueToAnyJSON :: Aeson.Value -> FC.AnyJSON
valueToAnyJSON value =
  case value of
    Aeson.String t -> FC.mkJSONText t
    Aeson.Bool b -> FC.mkJSONBool b
    Aeson.Number n -> FC.mkJSONNumber n
    Aeson.Array vector -> FC.mkJSONArray (fmap valueToAnyJSON (Vector.toList vector))
    Aeson.Object keyMap -> FC.mkJSONObject (keyMapToMap keyMap)
    Aeson.Null -> FC.mkJSONNull

keyMapToMap :: KeyMap.KeyMap Aeson.Value -> Map.Map T.Text FC.AnyJSON
keyMapToMap =
  fmap valueToAnyJSON
    . Map.mapKeys Key.toText
    . KeyMap.toMap
