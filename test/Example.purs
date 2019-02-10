module Example where
import Database.Mongo as Mongo
import Database.Mongo.Query (Query)
import Database.Mongo.Query as Q
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Process (exit)
import Prelude (Unit, bind, discard, show, ($))

main :: Effect Unit
main = launchAff_ $ do
  client <- Mongo.connect "mongodb://user:pass@host:port/db"
  let db = Mongo.db "db" client
  col <- Mongo.collection "item" db
  item <- Mongo.find searchQuery col
  log $ show item
  liftEffect $ exit 0

type Item = { id :: Int, name :: String, inner :: Inner  }

type Inner = { number :: Number } 

searchQuery :: Query Item
searchQuery = Q.or
  [ Q.by { id: Q.eq 26637 }
  , Q.by { inner: { number: Q.lte 10.0 } }
  ]
