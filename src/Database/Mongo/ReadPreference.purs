module Database.Mongo.ReadPreference where

import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ReadPreference :: Type

primary :: ReadPreference
primary = unsafeCoerce "primary"

primaryPreferred :: ReadPreference
primaryPreferred = unsafeCoerce "primaryPreferred"

secondary :: ReadPreference
secondary = unsafeCoerce "secondary"

secondaryPreferred :: ReadPreference
secondaryPreferred = unsafeCoerce "secondaryPreferred"

nearest :: ReadPreference
nearest = unsafeCoerce "nearest"

instance writeReadPreference :: WriteForeign ReadPreference where
  writeImpl = unsafeCoerce
