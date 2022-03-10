{ sources = [ "./src/**/*.purs" ]
, name = "mosaic"
, dependencies =
  [ "aff"
  , "canvas"
  , "colors"
  , "console"
  , "control"
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
  , "ordered-collections"
  , "painting"
  , "parallel"
  , "prelude"
  , "profunctor-lenses"
  , "refs"
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
