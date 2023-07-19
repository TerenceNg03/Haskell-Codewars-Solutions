module Solutions.Befunge.Spec (befungeSpec) where

import Solutions.Befunge (interpret)
import System.Random (newStdGen)
import Test.Hspec

befungeSpec :: Spec
befungeSpec = do
    describe "Befunge" $ do
        it "Example from description" $ do
            g <- newStdGen
            interpret
                g
                ">987v>.v\n\
                \v456<  :\n\
                \>321 ^ _@"
                `shouldBe` "123456789"
        it "Test Add" $ do
            g <- newStdGen
            interpret g ">12+::...@" `shouldBe` "333"
        it "Test wrap horizontal" $ do
            g <- newStdGen
            interpret
                g
                ">321v\n\
                \v   >\n\
                \>...@"
                `shouldBe` "123"
        it "Test wrap vertical" $ do
            g <- newStdGen
            interpret
                g
                ">321v \n\
                \    >^\n\
                \@... <"
                `shouldBe` "123"
        it "Test skip" $ do
            g <- newStdGen
            interpret
                g
                "99#g.@"
                `shouldBe` "9"
        it "Hello world" $ do
            g <- newStdGen
            interpret
                g
                ">25*\"!dlroW olleH\":v\n\
                \                v:,_@\n\
                \                >  ^"
                `shouldBe` "Hello World!\n"
        it "Quine" $ do
            g <- newStdGen
            interpret
                g
                "01->1# +# :# 0# g# ,# :# 5# 8# *# 4# +# -# _@"
                `shouldBe` "01->1# +# :# 0# g# ,# :# 5# 8# *# 4# +# -# _@"
        it "Sieve of Eratosthenes" $ do
            g <- newStdGen
            interpret
                g
                "2>:3g\" \"-!v\\  g30          <\n\
                \ |!`\"&\":+1_:.:03p>03g+:\"&\"`|\n\
                \ @               ^  p3\\\" \":<\n\
                \2 2345678901234567890123456789012345678"
                `shouldBe` "2 3 5 7 11 13 17 19 23 29 31 37 "
        it "Random directions" $ do
            g <- newStdGen
            interpret
                g
                "v@.<\n\
                \ >1^\n\
                \>?<^\n\
                \ >2^"
                `shouldSatisfy` (\xs -> xs == "1" || xs == "2")
