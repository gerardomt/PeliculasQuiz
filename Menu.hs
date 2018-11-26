module Menu where

import Data.Char
import System.IO

import Reading
import Texto

--Manejo del Menú Principal
menuPrincipal = do
  putStrLn "Bienvenido, selecciona una opción:"
  putStrLn "[1] Jugar"
  putStrLn "[2] Instrucciones"
  putStrLn "[3] Salir \10060"
  inst <- getLine
  opciones inst

--Control de opciones para el menú principal
opciones :: String -> IO()
opciones s
 | n==1 = menuSec 0 5
 | n==2 = instrucciones
 | n==3 = salir
 | otherwise = do
     putStrLn "Esa no es una opción válida"
     putStrLn ""
     menuPrincipal
  where n = read s :: Int

--Manejo del Menú de Juego
menuSec :: Int -> Int-> IO()
menuSec correctas incorrectas= do
  putStrLn $vidas incorrectas
  putStrLn "¿Cuál es el nombre de la película?"
  (q,p) <- quiz
  putStrLn p
  respuesta <- getLine
  if (compara respuesta q)
    then do
      putStrLn "Correcto \9989"
      putStrLn ("Respuestas correctas: "++[intToDigit $succ correctas])
      seguir (succ correctas) incorrectas
    else do
      putStrLn ("Incorrecto \10060: " ++ q)
      putStrLn ("Respuestas correctas: "++[intToDigit correctas])
      if ((pred incorrectas) == 0)
        then gameOver correctas
        else seguir correctas (pred incorrectas)


-- Pregunta al jugador si quiere seguir jugando
seguir :: Int -> Int -> IO()
seguir correcto incorrectas = do
  putStrLn "¿Quieres seguir jugando? (s/n)"
  sigue <- getLine
  if (toLower (head sigue) == 's')
  then do
    putStrLn ""
    menuSec correcto incorrectas
  else menuPrincipal


--Muestra las Instrucciones
instrucciones = do
  putStrLn "Instrucciones: \n\n   Se te presentarán unos emojis que servirán como pistas. A partir de \nellos deberás tratar de adivinar la película a la que éstos se refieren. \nDebes escribir el nombre de la película y oprimir enter, y el juego te\ndirá si acertaste o no. No te preocupes por mayúsculas, espacios o signos \nde puntuación, éstos no se tomarán en cuenta. Sólo puedes equivocarte\n5 veces. \n\n Presiona enter para continuar"
  seguir <- getLine
  menuPrincipal

-- Fin del juego
gameOver :: Int -> IO()
gameOver correctas = do
  putStrLn "\nGAME OVER \128546"
  putStrLn ("Tuviste "++[intToDigit correctas] ++ " respuestas correctas\n")
  menuPrincipal

--Devuelve un mensaje de salida
salir = do
  putStrLn "Adios \9995"

--Compara dos cadenas sin tomar en cuenta mayúsculas, espacios y signos
compara :: String -> String -> Bool
compara a b = (formato a) == (formato b)

-- Devuelve una cadena con n corazones
vidas :: Int -> String
vidas 0 = ""
vidas n = "\9829\65039 " ++ (vidas $pred n)
