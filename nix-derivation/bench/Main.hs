import Criterion (Benchmark)

import qualified Criterion
import qualified Criterion.Main
import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Nix.Derivation

main :: IO ()
main = Criterion.Main.defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
    [ Criterion.Main.env
        (Data.Text.Lazy.IO.readFile "tests/example1.drv")
        bench0
    ]
  where
    bench0 example =
        Criterion.bench "example" (Criterion.nf parseExample example)

    parseExample =
        Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation
