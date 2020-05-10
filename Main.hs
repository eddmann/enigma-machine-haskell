import Machine (encodeMessage, rotorIII, rotorII, rotorI, reflectorA, reflectorB, reflectorC)

main :: IO ()
main = print $ encodeMessage (rotorIII, rotorII, rotorI) reflectorB [] "HELLOWORLD"
