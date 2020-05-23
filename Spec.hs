import Test.Hspec
import Test.QuickCheck
import Control.Monad

import Machine

genReflector :: Gen Reflector
genReflector = elements [reflectorA, reflectorB, reflectorB]

genRotor :: Gen Rotor
genRotor = elements [rotorI, rotorII, rotorIII]

genRotors :: Gen Rotors
genRotors = liftM3 (,,) genRotor genRotor genRotor

plugboard :: [Char] -> [(Char, Char)]
plugboard zs = zip xs ys
    where (xs, ys) = splitAt ((length zs + 1) `div` 2) zs

genPlugboard :: Gen Plugboard
genPlugboard = fmap plugboard $ shuffle alphabet

genMessage :: Gen String
genMessage = listOf $ elements alphabet

main :: IO ()
main = hspec $ do
    describe "Machine" $ do
        it "encodes message without plugboard" $ do
            encodeMessage (rotorIII, rotorII, rotorI) reflectorB [] "HELLOWORLD" `shouldBe` "ILBDAAMTAZ"
        it "encodes message with plugboard" $ do
            encodeMessage (rotorIII, rotorII, rotorI) reflectorB [('A', 'B')] "HELLOWORLD" `shouldBe` "ILADBBMTBZ"
        it "handles second rotor step" $ do
            encodeMessage (rotorIII, rotorII, rotorI) reflectorB [] ([1..102] >> "A") `shouldBe` "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTHEORXPQPKOVHCBUBTZSZSOOSTGOTFSODBBZZLXLCYZXIFGWFDZEEQIB"
        it "encoded cipher matches message" $ do
            property $ forAll genReflector $ \reflector ->
                       forAll genRotors $ \rotors ->
                       forAll genPlugboard $ \plugboard ->
                       forAll genMessage $ \message ->
                          let cipher = encodeMessage rotors reflector plugboard message in
                              message == encodeMessage rotors reflector plugboard cipher
        it "cipher is same length as message" $ do
            property $ forAll genReflector $ \reflector ->
                       forAll genRotors $ \rotors ->
                       forAll genPlugboard $ \plugboard ->
                       forAll genMessage $ \message ->
                          let cipher = encodeMessage rotors reflector plugboard message in
                              length message == length cipher
