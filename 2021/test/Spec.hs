import Test.Tasty

import qualified DiveSpec              (tests)
import qualified BinaryDiagnosticsSpec (tests)

main :: IO ()
main = defaultMain $ testGroup "unit tests"
    [   DiveSpec.tests
    ,   BinaryDiagnosticsSpec.tests
    ]
