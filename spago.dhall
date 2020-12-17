{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "aeserialization-purs"
, dependencies =
  [ "arraybuffer"
  , "b64"
  , "base58"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "lists"
  , "node-buffer"
  , "numbers"
  , "psci-support"
  , "purescript-erlps-core"
  , "purescript-erlps-stdlib"
  , "rationals"
  , "b64"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
