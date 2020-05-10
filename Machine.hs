module Machine where

import Data.Tuple

alphabet = ['A'..'Z']

data Rotor = Rotor { _out :: String, _in :: String, _step :: Char }
    deriving (Show)

rotorI = Rotor "EKMFLGDQVZNTOWYHXUSPAIBRCJ" alphabet 'Q'
rotorII = Rotor "AJDKSIRUXBLHWTMCQGZNPYFVOE" alphabet 'E'
rotorIII = Rotor "BDFHJLCPRTXVZNYEIWGAKMUSQO" alphabet 'V'

type Reflector = [(Char, Char)]

reflectorA = zip alphabet "EJMZALYXVBWFCRQUONTSPIKHGD"
reflectorB = zip alphabet "YRUHQSLDPXNGOKMIEBFZCWVJAT"
reflectorC = zip alphabet "FVPJIAOYEDRZXWGCTKUQSBNMHL"

step :: Rotor -> Bool
step rotor = _step rotor == head (_in rotor)

invert :: Rotor -> Rotor
invert (Rotor _out _in _step) = Rotor _in _out _step

rotate :: Rotor -> Rotor
rotate (Rotor _out _in _step) = Rotor (tail _out ++ [head _out]) (tail _in ++ [head _in]) _step

type Rotors = (Rotor, Rotor, Rotor)

rotateAll :: Rotors -> Rotors
rotateAll (a, b, c) = (rotate a, if step a || step b then rotate b else b, if step b then rotate c else c)

passthrough :: Rotor -> Char -> Char
passthrough rotor letter = maybe '?' id (lookup letter in' >>= (flip lookup) out')
    where in' = zip alphabet (_out rotor)
          out' = zip (_in rotor) alphabet

passthroughAll :: Rotors -> Char -> Char
passthroughAll (a, b, c) letter = passthrough c . passthrough b . passthrough a $ letter

invertedPassthroughAll :: Rotors -> Char -> Char
invertedPassthroughAll (a, b, c) letter = passthrough (invert a) . passthrough (invert b) . passthrough (invert c) $ letter

plug :: Plugboard -> Char -> Char
plug plugboard letter = case lookup letter (plugboard ++ map swap plugboard) of
    Just plug -> plug
    Nothing -> letter

reflect :: Reflector -> Char -> Char
reflect reflector letter = maybe '?' id $ lookup letter reflector

encode :: Rotors -> Reflector -> Plugboard -> Char -> Char
encode rotors reflector plugboard letter =
    plug plugboard . invertedPassthroughAll rotors . reflect reflector . passthroughAll rotors . plug plugboard $ letter

type Plugboard = [(Char, Char)]

encodeMessage :: Rotors -> Reflector -> Plugboard -> [Char] -> [Char]
encodeMessage rotors reflector plugboard "" = ""
encodeMessage rotors reflector plugboard (letter : letters) =
    let rotated = rotateAll rotors in
        encode rotated reflector plugboard letter : encodeMessage rotated reflector plugboard letters
