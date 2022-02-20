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
  , "foreign-object"
  , "maybe"
  , "milkis"
  , "node-buffer"
  , "node-fs-aff"
  , "partial"
  , "prelude"
  , "psci-support"
  , "record"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
