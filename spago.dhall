{ name = "aeserialization-purs"
, dependencies =
  [ "arraybuffer"
  , "b64"
  , "base58"
  , "bigints"
  , "console"
  , "debug"
  , "effect"
  , "integers"
  , "lists"
  , "node-buffer"
  , "numbers"
  , "psci-support"
  , "purescript-erlps-core"
  , "purescript-erlps-stdlib"
  , "rationals"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
