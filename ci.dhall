let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/pending/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.matrixSteps
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC8104
              , haskellCi.GHC.GHC884
              , haskellCi.GHC.GHC865
              , haskellCi.GHC.GHC901
              ]
            , cabal = [ haskellCi.Cabal.Cabal32 ]
            }
        )
    : haskellCi.CI.Type
