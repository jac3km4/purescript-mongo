module Database.Mongo.Options
  ( InsertOptions()
  , defaultInsertOptions
  , UpdateOptions()
  , defaultUpdateOptions
  ) where

import Data.Maybe (Maybe(..))
import Database.Mongo.WriteConcern (WriteConcern)
import Simple.JSON (class WriteForeign, write)

-- | Typed options for inserting documents into a collection
newtype InsertOptions = InsertOptions
  { writeConcern :: Maybe WriteConcern
  , journaled    :: Maybe Boolean
  }

defaultInsertOptions :: InsertOptions
defaultInsertOptions = InsertOptions
  { writeConcern : Nothing
  , journaled    : Just false
  }

instance encodeJsonInsertOptions :: WriteForeign InsertOptions where
  writeImpl (InsertOptions {writeConcern, journaled}) =
    write { w: writeConcern, j: journaled }

-- | Typed options for updating documents into a collection
newtype UpdateOptions = UpdateOptions
  { writeConcern :: Maybe WriteConcern
  , journaled    :: Maybe Boolean
  , upsert       :: Maybe Boolean
  }

defaultUpdateOptions :: UpdateOptions
defaultUpdateOptions = UpdateOptions
  { writeConcern : Nothing
  , journaled    : Just false
  , upsert       : Just false
  }

instance encodeJsonUpdateOptions :: WriteForeign UpdateOptions where
  writeImpl (UpdateOptions o) =
    write { w: o.writeConcern, j: o.journaled, upsert: o.upsert }
