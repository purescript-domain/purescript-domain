{ name = "purescri.pt"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "console"
  , "control"
  , "dotenv"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "maybe"
  , "milkis"
  , "node-buffer"
  , "node-fs-aff"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
