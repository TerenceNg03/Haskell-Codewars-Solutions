module Solutions.RangeExtractor.Spec (rangeExtractorSpec) where

import Solutions.RangeExtractor (solution)
import Test.Hspec

rangeExtractorSpec :: Spec
rangeExtractorSpec = do
    describe "Example testing" $ do
        it "[-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]" $
            do
                solution [-6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
                `shouldBe` "-6,-3-1,3-5,7-11,14,15,17-20"
