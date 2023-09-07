{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq (NFData, force)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, nf)

import qualified Data.Hermes as H
import qualified Fleece.Aeson as FA
import Fleece.Core
  ( Fleece
  , NothingEncoding (EmitNull)
  , boolean
  , constructor
  , double
  , int
  , list
  , object
  , optionalNullable
  , required
  , text
  , (#+)
  )
import qualified Fleece.Core as FC
import qualified Fleece.Hermes as FH

main :: IO ()
main =
  defaultMain
    [ env (force <$> BS.readFile "json/persons9000.json") $ \strBs ->
        env (BSL.readFile "json/persons9000.json") $ \lazyBs ->
          bgroup
            "Decode 9000 Persons"
            [ bench "hermes-json" $
                nf (H.decodeEither $ H.list decodePerson) strBs
            , bench "json-fleece-hermes" $
                nf (FH.decode $ FC.list personSchema) strBs
            , bench "json-fleece-aeson" $
                nf (FA.decode $ FC.list personSchema) lazyBs
            ]
    ]

data Person = Person
  { _id :: Text
  , index :: Int
  , guid :: Text
  , isActive :: Bool
  , balance :: Text
  , picture :: Maybe Text
  , age :: Int
  , eyeColor :: Text
  , name :: Text
  , gender :: Text
  , company :: Text
  , email :: Text
  , phone :: Text
  , address :: Text
  , about :: Text
  , registered :: Text
  , latitude :: Double
  , longitude :: Double
  , tags :: [Text]
  , friends :: [Friend]
  , greeting :: Maybe Text
  , favoriteFruit :: Text
  }
  deriving stock (Show, Generic)

instance NFData Person

instance Aeson.FromJSON Person where
  parseJSON = FA.toParser personSchema

personSchema :: Fleece schema => schema Person
personSchema =
  object $
    constructor Person
      #+ required "_id" _id text
      #+ required "index" index int
      #+ required "guid" guid text
      #+ required "isActive" isActive boolean
      #+ required "balance" balance text
      #+ optionalNullable EmitNull "picture" picture text
      #+ required "age" age int
      #+ required "eyeColor" eyeColor text
      #+ required "name" name text
      #+ required "gender" gender text
      #+ required "company" company text
      #+ required "email" email text
      #+ required "phone" phone text
      #+ required "address" address text
      #+ required "about" about text
      #+ required "registered" registered text
      #+ required "latitude" latitude double
      #+ required "longitude" longitude double
      #+ required "tags" tags (list text)
      #+ required "friends" friends (list friendSchema)
      #+ optionalNullable EmitNull "greeting" greeting text
      #+ required "favoriteFruit" favoriteFruit text

friendSchema :: Fleece schema => schema Friend
friendSchema =
  object $
    constructor Friend
      #+ required "id" fId int
      #+ required "name" fName text

data Friend = Friend
  { fId :: Int
  , fName :: Text
  }
  deriving stock (Show, Generic)

instance NFData Friend

instance Aeson.FromJSON Friend where
  parseJSON = FA.toParser friendSchema

decodePerson :: H.Decoder Person
decodePerson =
  H.object $
    Person
      <$> H.atKey "_id" H.text
      <*> H.atKey "index" H.int
      <*> H.atKey "guid" H.text
      <*> H.atKey "isActive" H.bool
      <*> H.atKey "balance" H.text
      <*> H.atKey "picture" (H.nullable H.text)
      <*> H.atKey "age" H.int
      <*> H.atKey "eyeColor" H.text
      <*> H.atKey "name" H.text
      <*> H.atKey "gender" H.text
      <*> H.atKey "company" H.text
      <*> H.atKey "email" H.text
      <*> H.atKey "phone" H.text
      <*> H.atKey "address" H.text
      <*> H.atKey "about" H.text
      <*> H.atKey "registered" H.text
      <*> H.atKey "latitude" H.double
      <*> H.atKey "longitude" H.double
      <*> H.atKey "tags" (H.list H.text)
      <*> H.atKey "friends" (H.list decodeFriend)
      <*> H.atKey "greeting" (H.nullable H.text)
      <*> H.atKey "favoriteFruit" H.text

decodeFriend :: H.Decoder Friend
decodeFriend =
  H.object $
    Friend
      <$> H.atKey "id" H.int
      <*> H.atKey "name" H.text
