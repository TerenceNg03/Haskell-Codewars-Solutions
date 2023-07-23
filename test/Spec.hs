import Solutions.Befunge.Spec (befungeSpec)
import Solutions.RangeExtractor.Spec (rangeExtractorSpec)
import Solutions.Spiral.Spec (spiralSpec)
import Test.Hspec

main :: IO ()
main = do
    hspec befungeSpec
    hspec spiralSpec
    hspec rangeExtractorSpec
