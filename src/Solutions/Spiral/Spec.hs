module Solutions.Spiral.Spec (spiralSpec) where

import Solutions.Spiral
import Test.Hspec

-- You may use this type to display your output.
newtype ShowSpiral = ShowSpiral [[Int]] deriving (Eq)

instance Show ShowSpiral where
    show (ShowSpiral spiral) =
        unlines $
            ["<samp>"]
                ++ [[if x == 1 then '0' else '.' | x <- row] | row <- spiral]
                ++ ["</samp>"]

spiralSpec :: Spec
spiralSpec = do
    describe "Spiral" $ do
        it "Size 5" $
            ShowSpiral (spiralize 5)
                `shouldBe` ShowSpiral
                    [ [1, 1, 1, 1, 1]
                    , [0, 0, 0, 0, 1]
                    , [1, 1, 1, 0, 1]
                    , [1, 0, 0, 0, 1]
                    , [1, 1, 1, 1, 1]
                    ]
        it "Size 8" $
            ShowSpiral (spiralize 8)
                `shouldBe` ShowSpiral
                    [ [1, 1, 1, 1, 1, 1, 1, 1]
                    , [0, 0, 0, 0, 0, 0, 0, 1]
                    , [1, 1, 1, 1, 1, 1, 0, 1]
                    , [1, 0, 0, 0, 0, 1, 0, 1]
                    , [1, 0, 1, 0, 0, 1, 0, 1]
                    , [1, 0, 1, 1, 1, 1, 0, 1]
                    , [1, 0, 0, 0, 0, 0, 0, 1]
                    , [1, 1, 1, 1, 1, 1, 1, 1]
                    ]
        it "Size 13" $
            ShowSpiral (spiralize 13)
                `shouldBe` ShowSpiral
                    [ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
                    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
                    , [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1]
                    , [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1]
                    , [1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1]
                    , [1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1]
                    , [1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1]
                    , [1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1]
                    , [1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1]
                    , [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1]
                    , [1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1]
                    , [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
                    , [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
                    ]
