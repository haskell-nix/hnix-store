let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/main/haskell-ci.dhall

let defSteps = haskellCi.defaultCabalSteps

in    haskellCi.generalCi
        ( haskellCi.withNix
            ( defSteps
              with docStep = None haskellCi.BuildStep
              with extraSteps.pre
                   =
                    defSteps.extraSteps.pre
                  # [ haskellCi.installCachixStep "hnix-store"
                    , haskellCi.BuildStep.NameIf
                        { name = "Allow unprivileged userns"
                        , run =
                            "sudo sysctl kernel.apparmor_restrict_unprivileged_userns=0"
                        , `if` = "matrix.os == 'ubuntu-latest'"
                        }
                    ]
            )
        )
        haskellCi.DhallMatrix::{
        , ghc = [ haskellCi.GHC.GHC9102, haskellCi.GHC.GHC984 ]
        , os = [ haskellCi.OS.Ubuntu, haskellCi.OS.MacOS ]
        }
    : haskellCi.CI.Type
