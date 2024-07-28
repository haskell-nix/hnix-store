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
                  # [ haskellCi.installCachixStep "hnix-store" ]
            )
        )
        haskellCi.DhallMatrix::{
        , ghc = [ haskellCi.GHC.GHC982, haskellCi.GHC.GHC966 ]
        , os = [ haskellCi.OS.Ubuntu, haskellCi.OS.MacOS ]
        }
    : haskellCi.CI.Type
