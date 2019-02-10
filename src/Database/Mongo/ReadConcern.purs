module Database.Mongo.ReadConcern where

import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ReadConcern :: Type

local :: ReadConcern
local = unsafeCoerce "local"

available :: ReadConcern
available = unsafeCoerce "available"

majority :: ReadConcern
majority = unsafeCoerce "majority"

linearizable :: ReadConcern
linearizable = unsafeCoerce "linearizable"

snapshot :: ReadConcern
snapshot = unsafeCoerce "snapshot"

instance writeReadConcern :: WriteForeign ReadConcern where
  writeImpl = unsafeCoerce
