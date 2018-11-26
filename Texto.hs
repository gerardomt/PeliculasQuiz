module Texto where

import Data.Char
import Data.Text (pack, unpack, replace)

minusculas :: String -> String
minusculas s = map toLower s

espacios :: String -> String
espacios s = filter (\x -> not $ isSpace x) s

puntuacion :: String -> String
puntuacion s = filter (\x -> not $ isPunctuation x) s

remplaza :: String -> String -> String -> String
remplaza e r t = unpack (replace (pack e) (pack r) (pack t))

formato :: String -> String
formato s = minusculas $ espacios $ puntuacion s
--formato s = espacios $puntuacion $remplaza "an " "" $remplaza "a " "" $remplaza "the " "" $minusculas s
