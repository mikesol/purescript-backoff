{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "free"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
