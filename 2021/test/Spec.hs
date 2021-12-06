import Test.Tasty

import qualified DiveSpec              ( tests )
import qualified BinaryDiagnosticsSpec ( tests )
import qualified BingoSpec             ( tests )
import qualified HydrothermalVentsSpec ( tests )
import qualified LanternfishSpec       ( tests )

main :: IO ()
main = defaultMain $ testGroup "unit tests"
    [ DiveSpec.tests
    , BinaryDiagnosticsSpec.tests
    , BingoSpec.tests
    , HydrothermalVentsSpec.tests
    , LanternfishSpec.tests
    ]
