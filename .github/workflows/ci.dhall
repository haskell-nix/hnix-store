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
        , ghc =
          [ haskellCi.GHC.GHC963
          , haskellCi.GHC.GHC947
          , haskellCi.GHC.GHC928
          , haskellCi.GHC.GHC902
          , haskellCi.GHC.GHC8107
          ]
        , os = [ haskellCi.OS.Ubuntu, haskellCi.OS.MacOS ]
        }
    : haskellCi.CI.Type
