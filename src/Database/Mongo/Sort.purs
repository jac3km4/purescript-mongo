module Database.Mongo.Sort where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Sort :: Type

asc :: String -> Sort
asc field = unsafeCoerce [ field, unsafeCoerce 1 ]

desc :: String -> Sort
desc field = unsafeCoerce [ field, unsafeCoerce 0 ]
