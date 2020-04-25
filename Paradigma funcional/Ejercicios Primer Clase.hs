import Text.Show.Functions

--1) Definir la función calcular, que recibe una tupla de 2 elementos, y devuelve una nueva tupla según las siguientes reglas:
--si el primer elemento es par lo duplica; si no lo deja como está
--si el segundo elemento es impar le suma 1; si no deja como está

calcular :: (Integer, Integer) -> (Integer, Integer)
calcular (a, b) =  (duplicaSiEsPar a, sumaUnoSiEsImpar b)
duplicaSiEsPar :: Integer -> Integer
duplicaSiEsPar elemento | even elemento = elemento * 2
                        | otherwise = elemento
sumaUnoSiEsImpar :: Integer -> Integer
sumaUnoSiEsImpar elemento | not (even elemento) = elemento + 1
                          | otherwise = elemento

---------------------------------------------------------------------------------------------------------------------------------------

-- 2) Definir las funciones boolenas estándar. Sin usar las funciones predefinidas.

funcionAnd :: Bool -> Bool -> Bool
funcionAnd valor otroValor | not valor = False
                           | not otroValor = False
                           | otherwise = True

funcionOr :: Bool -> Bool -> Bool
funcionOr valor otroValor | valor = True
                          | otroValor = True
                          | otherwise = False

----------- RESOLVER ESTO CON PATTER MATCHING

funcionOr' :: Bool -> Bool -> Bool
funcionOr' True _ = True
funcionOr' _ segundoValor = segundoValor

funcionAnd' :: Bool -> Bool -> Bool
funcionAnd' True segundoValor = segundoValor
funcionAnd' _ _ = False


------------------------------------------------------------------------------------------------------------------------

--3) Definir la función notaMaxima que dado un alumno devuelva la máxima nota del alumno. (Nota resolverlo sin guardas).

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

maximoEntreTres :: Integer -> Integer -> Integer -> Integer
maximoEntreTres numero numero' numero'' = ((max numero).max numero') numero''

notaMaxima :: Alumno -> Integer
notaMaxima (_, nota, nota', nota'') = maximoEntreTres nota nota' nota''

-------------------------------------------------------------------------------------------------------------------------

-- 4) Definir la función cuadruple reutilizando la función doble. (usar composición)

doble :: Integer -> Integer 
doble numero = numero * 2

cuadruple :: Integer -> Integer
cuadruple numero = (doble.doble) numero

-------------------------------------------------------------------------------------------------------------------------

-- 5) Definir la función esMayorA, que verifique si el doble del siguiente 
--    de la suma entre 2 y un número es mayor a 10. (usar composición)

esMayorA :: Integer -> Bool
esMayorA numero =   ((> 10).doble.siguiente.(+ 2)) numero

siguiente :: Integer -> Integer
siguiente numero = numero + 1

-------------------------------------------------------------------------------------------------------------------------

--6) Dar expresiones lambda que sean equivalentes a las siguientes expresiones:
--  triple
--  siguiente
--  suma
--  sumarDos

triple = (\ x -> x * 3)
siguiente' = (\ x -> x + 1)
suma = (\ x y -> x + y)
sumarDos = (\ x -> suma x 2)

-- 7)  Dada las siguientes definiciones:
--7.1) apply f x = f x
--¿ A qué se reduce la siguiente expresión ?.
--> apply fst  (const 5 7, 4)
 
--7.2) twice f x = (f . f) x
--¿ A qué se reduce la siguiente expresión ?.
-->twice (`div` 2) 12

Ambas expresiones no están dentro del scope.
