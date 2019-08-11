{ name =
    "codec"
, dependencies =
    [ "effect"
    , "exceptions"
    , "generics-rep"
    , "bytestring"
    , "longs"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
