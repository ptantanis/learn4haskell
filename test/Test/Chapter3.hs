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
    describe "Castle" $ do
        let myHouse = House { housePopulation = Four }
            myCity = City {
                cityCastle = NoCastle
                , cityWall = False
                , cityBuilding = Church
                , cityHouses = [myHouse]
            }
        describe "buildCastle" $ do
            it "build castle in city, given no castle" $ castleName (cityCastle (buildCastle "new castle" myCity)) `shouldBe` "new castle"
            it "replace castle in city, given existed castle" $ castleName (cityCastle (buildCastle "new castle" myCity { cityCastle = Castle "old castle" })) `shouldBe` "new castle"
        describe "buildHouse" $ do
            let newHouses = cityHouses (buildHouse One myCity)
            it "add new house" $ length newHouses `shouldBe` 2 
            it "got new house with specific number of people" $ housePopulationValue (housePopulation (newHouses !! 0)) `shouldBe` 1
        describe "buildWall" $ do
            let 
                canBuildWallCity =  myCity { 
                    cityCastle = Castle "New" 
                    , cityHouses = [myHouse, myHouse, myHouse]
                } 
            it "build wall for city" $ cityWall (buildWall canBuildWallCity) `shouldBe` True
            it "do not destroy existed wall" $ cityWall (buildWall myCity { cityWall = True }) `shouldBe` True
            it "when no castle, should not build wall" $ cityWall (buildWall canBuildWallCity { cityCastle = NoCastle }) `shouldBe` False
            it "when not enough population, should not build wall" $ cityWall (buildWall canBuildWallCity { cityHouses = [myHouse, myHouse, House { housePopulation = One }] }) `shouldBe` False