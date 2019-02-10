{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "effect"
    , "aff"
    , "console"
    , "simple-json"
    , "psci-support"
    , "debug"
    , "node-process"
    ]
, packages =
    ./packages.dhall
}
