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
        describe "Append" $ do
            it "should added Gold value" $ unGold (append (Gold 1) (Gold 2)) `shouldBe` 3
            it "should concatenate List" $ length (append ["1"] ["2"]) `shouldBe` 2
            it "should got Nothing when append Nothing" $  append Nothing (Just "") `shouldBe` Nothing
            it "should got result when append Maybe" $  append (Just "a") (Just "b") `shouldBe` Just "ab"
    describe "DayOfWeek" $ do
        describe "isWeekend" $ do
            it "given Sun, return True" $ isWeekend Sun `shouldBe` True
            it "given Sat, return True" $ isWeekend Sat `shouldBe` True
        describe "nextDay" $ do
            it "given Sun, return Mon" $ nextDay Sun `shouldBe` Mon
            it "given Wed, return Thu" $ nextDay Wed `shouldBe` Thu
            it "given Sat, return Sun" $ nextDay Sat `shouldBe` Sun
        describe "dayToParty" $ do
            it "given Fri, return 0" $ dayToParty Fri `shouldBe` 0
            it "given Wed, return 2" $ dayToParty Wed `shouldBe` 2
            it "given Sun, return 5" $ dayToParty Sun `shouldBe` 5
            it "given Sat, return 6" $ dayToParty Sat `shouldBe` 6