module Machine where

import Data.Tuple (swap)

alphabet :: [Char]
alphabet = ['A'..'Z']

data Rotor = Rotor { _out :: String, _in :: String, _step :: Char }
    deriving (Show)

type Rotors = (Rotor, Rotor, Rotor)

rotorI :: Rotor
rotorI = Rotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" alphabet 'Q'

rotorII :: Rotor
rotorII = Rotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" alphabet 'E'

rotorIII :: Rotor
rotorIII = Rotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" alphabet 'V'

toRotor :: Char -> Maybe Rotor
toRotor = (flip lookup) [('1', rotorI), ('2', rotorII), ('3', rotorIII)]

isStep :: Rotor -> Bool
isStep rotor = _step rotor == head (_in rotor)

invert :: Rotor -> Rotor
invert (Rotor _out _in _step) = Rotor _in _out _step

rotate :: Rotor -> Rotor
rotate (Rotor _out _in _step) = Rotor (tail _out ++ [head _out]) (tail _in ++ [head _in]) _step

rotateAll :: Rotors -> Rotors
rotateAll (a, b, c) = (rotate a, if isStep a || isStep b then rotate b else b, if isStep b then rotate c else c)

passthrough :: Rotor -> Char -> Char
passthrough rotor letter = maybe '?' id (lookup letter in' >>= (flip lookup) out')
    where in' = zip alphabet (_out rotor)
          out' = zip (_in rotor) alphabet

passthroughAll :: Rotors -> Char -> Char
passthroughAll (a, b, c) letter = passthrough c . passthrough b . passthrough a $ letter

invertedPassthroughAll :: Rotors -> Char -> Char
invertedPassthroughAll (a, b, c) letter = passthrough (invert a) . passthrough (invert b) . passthrough (invert c) $ letter

type Reflector = [(Char, Char)]

reflectorA :: Reflector
reflectorA = zip alphabet "EJMZALYXVBWFCRQUONTSPIKHGD"

reflectorB :: Reflector
reflectorB = zip alphabet "YRUHQSLDPXNGOKMIEBFZCWVJAT"

reflectorC :: Reflector
reflectorC = zip alphabet "FVPJIAOYEDRZXWGCTKUQSBNMHL"

toReflector :: Char -> Maybe Reflector
toReflector = (flip lookup) [('A', reflectorA), ('B', reflectorB), ('C', reflectorC)]

reflect :: Reflector -> Char -> Char
reflect reflector letter = maybe '?' id $ lookup letter reflector

type Plugboard = [(Char, Char)]

plug :: Plugboard -> Char -> Char
plug plugboard letter = maybe letter id $ lookup letter plugboard'
    where plugboard' = (plugboard ++ map swap plugboard)

encode :: Rotors -> Reflector -> Plugboard -> Char -> Char
encode rotors reflector plugboard letter =
    plug plugboard . invertedPassthroughAll rotors . reflect reflector . passthroughAll rotors . plug plugboard $ letter

encodeMessage :: Rotors -> Reflector -> Plugboard -> String -> String
encodeMessage _ _ _ "" = ""
encodeMessage rotors reflector plugboard (letter : letters) =
    let rotated = rotateAll rotors in
        encode rotated reflector plugboard letter : encodeMessage rotated reflector plugboard letters
