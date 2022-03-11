{ sources = [ "./src/**/*.purs" ]
, name = "mosaic"
, dependencies =
  [ "aff"
  , "arrays"
  , "behaviors"
  , "canvas"
  , "colors"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "event"
  , "foldable-traversable"
  , "foreign-object"
  , "free"
  , "halogen"
  , "homogeneous"
  , "identity"
  , "indexed-monad"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "painting"
  , "parallel"
  , "prelude"
  , "profunctor-lenses"
  , "refs"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "unsafe-coerce"
  , "wags"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
}
