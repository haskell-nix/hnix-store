let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

let defSteps = haskellCi.defaultCabalSteps

in    haskellCi.generalCi
        ( haskellCi.withNix
            ( defSteps
              with extraSteps.pre
                   =
                    defSteps.extraSteps.pre
                  # [ haskellCi.installCachixStep "hnix-store"
                    , haskellCi.BuildStep.NameIf
                        { name =
                            "Install libsodium"
                        , run = "sudo apt install libsodium-dev"
                        , `if` = "matrix.os == 'ubuntu-latest'"
                        }
                    ]
            )
        )
        haskellCi.DhallMatrix::{
        , ghc =
          [ haskellCi.GHC.GHC963, haskellCi.GHC.GHC947, haskellCi.GHC.GHC902 ]
        , os = [ haskellCi.OS.Ubuntu, haskellCi.OS.MacOS ]
        }
    : haskellCi.CI.Type
