{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "effect"
    , "aff"
    , "simple-json"
    , "psci-support"
    , "node-process"
    ]
, packages =
    ./packages.dhall
}
