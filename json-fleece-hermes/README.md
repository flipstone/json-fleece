# json-fleece-hermes

Interpret a `Fleece` schema as a `Data.Hermes.Decoder`.

```haskell
ghci> :set -XOverloadedStrings
ghci> import qualified Fleece.Core as FC
ghci> import qualified Fleece.Hermes as FH
ghci> FH.decode (FC.list FC.text) "[\"hello\", \"world\"]"
Right ["hello","world"]
```

## Benchmarks

```
Decode 9000 Persons
  hermes-json:        OK (0.77s)
    49.8 ms ± 3.0 ms,  99 MB allocated,  55 MB copied, 111 MB peak memory
  json-fleece-hermes: OK (0.55s)
    78.9 ms ± 5.5 ms, 188 MB allocated,  70 MB copied, 111 MB peak memory
  json-fleece-aeson:  OK (0.78s)
    257  ms ± 7.2 ms, 880 MB allocated, 262 MB copied, 194 MB peak memory
```


## Limitations / Future Work

Having all numbers go through `Scientific` hurts performance. If Fleece offered
core semantics for common number types then we could use the faster hermes
versions.

Hermes cannot currently decode scalar documents. That means decoding anything
that isn't an object or an array will fail. This shouldn't be a deal-breaker
for this library's use case, but it can be confusing and leads to a smelly
`jsonString` implementation.

If `jsonString` is mainly intended for numbers as strings, I think Fleece
expressions like `intAsString`, `doubleAsString`, and `wordAsString` would be
better, as it would allow us to use simdjson:
https://github.com/simdjson/simdjson/blob/master/doc/basics.md#parsing-numbers-inside-strings.
These would have to be added to Hermes, as well.
