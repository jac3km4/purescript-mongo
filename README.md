# purescript-mongo
Typesafe Mongo library for Purescript

# usage
File `test/Example.purs` has an example demonstrating how to connect to a DB.

# queries

This library exposes a `Query` type that is parametrized by the type that it's meant to query against.
The `Database.Mongo.Query` module contains a DSL for building those.
By defining your schema and using it with the DSL you can ensure that your queries are correct.

Example:
```purs
import Database.Mongo.Query (Query)
import Database.Mongo.Query as Q

type Item = { id :: Int, name :: String, inner :: Inner  }

type Inner = { number :: Number } 

searchQuery :: Query Item
searchQuery = Q.or
  [ Q.by { id: Q.eq 26637 }
  , Q.by { inner: { number: Q.lte 10.0 } }
  ]
```

It is checked during compile-time whether all the fields exist and have correct types.
