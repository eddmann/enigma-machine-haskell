import Test.Hspec
import Test.QuickCheck

import Machine

genReflector :: Gen Reflector
genReflector = elements [reflectorA, reflectorB, reflectorB]

genRotor :: Gen Rotor
genRotor = elements [rotorI, rotorII, rotorIII]

genMessage :: Gen String
genMessage = listOf $ elements alphabet

genRotors :: Gen Rotors
genRotors = do
    a <- genRotor
    b <- genRotor
    c <- genRotor
    return (a, b, c)

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
                       forAll genMessage $ \message ->
                          let cipher = encodeMessage rotors reflector [] message in
                              message == encodeMessage rotors reflector [] cipher
        it "cipher is same length as message" $ do
            property $ forAll genReflector $ \reflector ->
                       forAll genRotors $ \rotors ->
                       forAll genMessage $ \message ->
                          let cipher = encodeMessage rotors reflector [] message in
                              length message == length cipher
