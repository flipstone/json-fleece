# Extensible JSON Schemas for Haskell

This library lets you specify a JSON schema that can be reused for *both* encoding, decoding, documentation and more.

For example:

```haskell
import qualified Fleece.Core as FC

data Dog = Dog
  { dogName :: Text
  , dogAge :: Maybe Int
  }

dogSchema :: FC.Fleece t => FC.Schema t Dog
dogSchema =
  FC.object $
    FC.constructor Dog
      #+ FC.required "name" dogName FC.text
      #+ FC.optional "age" dogAge FC.int
```

This schema would encode/decode to e.g. this JSON:

```json
{ "name": "Lassie"
, "age": 88
}
```
