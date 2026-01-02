# TaggedUnionExample

One of the following field structures, depending on the value found in the type field.

## type = person

When the "type" field has the value "person", the object has the following fields:

|Field|Key Required|Null Allowed|Type|
|---|---|---|---|
|name|yes|no|text|
|age|yes|no|number|

## type = company

When the "type" field has the value "company", the object has the following fields:

|Field|Key Required|Null Allowed|Type|
|---|---|---|---|
|name|yes|no|text|
|tooBigToFail|yes|no|boolean|

