module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    describe "Chapter3Normal" $ it "" $ True `shouldBe` True
    describe "Chapter3Advanced" $ it "" $ True `shouldBe` True
    describe "Fighter" $ do
        let myFighter = Fighter {
            fighterHealth = 100
            , fighterAttack = 100
            , fighterGold = 100
        }
        describe "isDefeated" $ do
            it "False when health more than 0" $ isDefeated myFighter `shouldBe` True
            it "True when health is 0" $ isDefeated myFighter { fighterHealth = 0  } `shouldBe` True
            it "True when health less than 0" $ isDefeated myFighter { fighterHealth = -1  } `shouldBe` True
        describe "attack" $ do
            it "got fighter with reduced health" $ fighterHealth (attack myFighter myFighter) `shouldBe` 0
