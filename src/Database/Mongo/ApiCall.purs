module Database.Mongo.ApiCall where

import Unsafe.Coerce (unsafeCoerce)

foreign import data ApiCall :: Type

scalar :: ApiCall
scalar = unsafeCoerce "One"

vector :: ApiCall
vector = unsafeCoerce "Many"
