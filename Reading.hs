module Reading where

import System.IO
import Data.String
import System.Random

-- Devuelve una tupla ("nombre", "pista_emoji") de el archivo
-- "dataSetMovieEmoji.txt" al azar
quiz = do
  contents <- readFile "dataSetMovieEmoji.txt"
  g <- newStdGen
  let listContents = lines contents
      tupleContents = (map (\x -> spliText x "" "" True) listContents)
      tupleContentsUnicode = (map (\x -> (fst x,unicodeE $snd x) ) tupleContents) 
      l =  length tupleContentsUnicode
      r = fst (randomR (0, l-1) g)
  return (tupleContentsUnicode !! r)

--Toma una línea de "dataSetMovieEmoji.txt", dos cadenas(para hacer
--recursión de cola), y un booleano(controla en cuál cadena se agregan
--los caracteres: True - primera, False - segunda), y devuelve una
--tupla ("nombre", "pista_emoji")
spliText :: String -> String -> String -> Bool -> (String,String)
spliText (')':xs) m n b = (m,n)
spliText ('\"':xs) m n b = joinC xs m n b
spliText (x:xs) m n b = spliText xs m n b

--Auxiliar de spliText
joinC :: String -> String -> String -> Bool -> (String,String)
joinC ('\"':xs) m n b = spliText xs m n False
joinC (x:xs) m n b
 | b =  joinC xs (m++[x]) n b
 | otherwise = joinC xs m (n++[x]) b

--Devuelve la cadena con representaciones Unicode
unicodeE :: String -> String
unicodeE s = read $ "\"" ++ s ++ "\""
