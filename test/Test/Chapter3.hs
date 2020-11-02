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
        describe "attackFatal" $ do
            it "got True when attacked and died" $ attackFatal myFighter myFighter `shouldBe` True
            it "got False when attacked and still alive" $ attackFatal myFighter myFighter { fighterHealth = 200 } `shouldBe` False
        describe "fight" $ do
            it "got combined tressure, when knight wins" $ fight myFighter myFighter `shouldBe` 200
            it "got -1, when knight losses" $ fight myFighter myFighter { fighterHealth = 200 } `shouldBe` -1
            it "got knight's tressure, when no winner" $ fight myFighter { fighterHealth = 200 } myFighter { fighterHealth = 200 } `shouldBe` 100