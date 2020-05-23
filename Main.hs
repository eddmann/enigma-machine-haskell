import System.Environment
import Data.Char

import Machine (Rotors, Plugboard, Reflector, toReflector, toRotor, encodeMessage)

parseRotors :: [String] -> IO Rotors
parseRotors ("-rotors" : [a, b, c] : _) = case (toRotor a, toRotor b, toRotor c) of
    (Just a', Just b', Just c') -> pure (a', b', c')
    _ -> fail $ "invalid rotors " ++ [a, b, c] ++ " specified in args"
parseRotors (_ : xs) = parseRotors xs
parseRotors [] = fail "rotors must be specified in args"

toPlugboard :: String -> Plugboard
toPlugboard (x : y : xs) = [(toUpper x, toUpper y)] ++ toPlugboard xs
toPlugboard _ = []

parsePlugboard :: [String] -> IO Plugboard
parsePlugboard ("-plugboard" : plugboard : _) = pure $ toPlugboard plugboard
parsePlugboard (_ : xs) = parsePlugboard xs
parsePlugboard [] = pure []

parseReflector :: [String] -> IO Reflector
parseReflector ("-reflector" : [reflector] : _) = case (toReflector . toUpper) reflector of
    Just reflector' -> pure reflector'
    Nothing -> fail $ "invalid reflector " ++ [reflector] ++ " specified in args"
parseReflector (_ : xs) = parseReflector xs
parseReflector [] = fail "reflector must be specified in args"

parseMessage :: [String] -> IO String
parseMessage ("-message" : message : _) = pure $ map toUpper message
parseMessage (_ : xs) = parseMessage xs
parseMessage [] = fail "message must be specified in args"

main :: IO ()
main =  do
    args <- System.Environment.getArgs
    rotors <- parseRotors args
    reflector <- parseReflector args
    plugboard <- parsePlugboard args
    message <- parseMessage args
    print $ encodeMessage rotors reflector plugboard message
