module Database.Mongo.WriteConcern where

import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data WriteConcern :: Type

nNodes :: Int -> WriteConcern
nNodes = unsafeCoerce

noAck :: WriteConcern
noAck = nNodes 0

oneAck :: WriteConcern
oneAck = nNodes 1

majority :: WriteConcern
majority = unsafeCoerce "majority"

instance writeWriteConcern :: WriteForeign WriteConcern where
  writeImpl = unsafeCoerce
