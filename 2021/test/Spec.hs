import Test.Tasty

import qualified DiveSpec              ( tests )
import qualified BinaryDiagnosticsSpec ( tests )
import qualified BingoSpec             ( tests )
import qualified HydrothermalVentsSpec ( tests )
import qualified LanternfishSpec       ( tests )
import qualified SegmentDisplaySpec    ( tests )
import qualified SmokeBasinSpec        ( tests )
import qualified SyntaxScoringSpec     ( tests )
import qualified OctopusSpec         ( tests )

main :: IO ()
main = defaultMain $ testGroup "unit tests"
    [ DiveSpec.tests
    , BinaryDiagnosticsSpec.tests
    , BingoSpec.tests
    , HydrothermalVentsSpec.tests
    , LanternfishSpec.tests
    , SegmentDisplaySpec.tests
    , SmokeBasinSpec.tests
    , SyntaxScoringSpec.tests
    , OctopusSpec.tests
    ]
