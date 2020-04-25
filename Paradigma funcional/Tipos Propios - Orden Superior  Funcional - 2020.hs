import Text.Show.Functions
import Data.List

-------- Constructores y data.
data Figura = Circulo { radio :: Double} | Rectangulo { base :: Double, altura :: Double} deriving Show

circulo = Circulo 7
rectangulo = Rectangulo 5 2

-- Area de una Figura
area :: Figura -> Double
area (Circulo radio) = 3.14 * radio^2
area (Rectangulo base altura) = base * altura

-- tipos propios Type
type Punto = (Double, Double)

distancia :: Punto -> Double
distancia (x, y) = sqrt (x ^ 2 + y ^ 2)

-- Pero para calcular la distancia de un punto de 3 dimensiones, lo anterior ya no me sirve, me conviene
-- tener un contstructor que me permita instanciar uno u otro:

data Punto' = Plano { coordenadaX :: Double, coordenadaY :: Double}
            | Espacio { coordenadaX :: Double, coordenadaY :: Double, coordenadaZ :: Double}

distancia' :: Punto' -> Double
distancia' (Plano x y) = distancia (x, y)
distancia' (Espacio x y z) = sqrt (x^2 + y^2 + z^2)


------------- LISTAS POR COMPRESIÓN -----------------
numerosPares :: [Integer] -> [Integer]   
numerosPares numeros = [ num | num <- numeros, even num]

-- Producto cartesiano entre LISTAS

productoCartesiano lista otraLista = [(i, j) | i <- lista, j <- otraLista]

--- Podemos armar una función, que reciba una lista, y seleccione los elementos en base a un criterio deseado:
-- FUNCIONES DE ORDEN SUPERIOR: QUE RECIBEN COMO PARÁMETRO UNA FUNCIÓN, O SI DEVUELVE UNA FUNCIÓN.
seleccionarSegunCriterio :: (Integer -> Bool) -> [Integer] -> [Integer]
seleccionarSegunCriterio funcion numeros = [num | num <- numeros , funcion num]

--------------------------------------------------------------------------------------------------------------

--Ejercicios práctica tipos propios - orden superior 

-- 1) Queremos calcular el sueldo de los empleados de nuestra empresa. Tenemos dos tipos de empleado:
-- ● Los comunes: nos interesa saber el sueldo básico y el nombre.
-- ● Los jerárquicos: nos interesa conocer el sueldo básico, la cantidad de gente a cargo y el nombre.
 
-- El sueldo que cobran los comunes se determina por el sueldo básico, en los empleados  jerárquicos se calcula como sueldo básico + plus por la cantidad de gente a cargo (500 por cada persona a cargo).


data Empleado = Comun { sueldoBasico :: Double, nombre :: String}
                | Jefe { sueldoBasico :: Double, genteACargo :: Double, nombre :: String}

sueldo (Comun sueldo _ ) = sueldo
sueldo (Jefe sueldo cga _) = sueldo + cga * 500

---------------------------------------------------------------------------------------------------------------
-- 2) 
-- Se conocen estas bebidas:
 
-- data Bebida = Cafe Nombre Azucar | Gaseosa Sabor Azucar

-- Dado un producto determinar si es energizante.
-- ●      Si es café es energizante si es un capuchino.
-- ●   Si es una gaseosa es energizante si es de sabor a pomelo y tiene más de 10gr de azúcar.

data Bebida = Cafe {nombre' :: String, grAzuzar :: Integer}
            |   Gaseosa {sabor :: String, grAzucar :: Integer} deriving Show

esLaMismaPalabra :: String -> String -> Bool
esLaMismaPalabra cadena otraCadena = (tieneMismaLongitud cadena otraCadena) && (cadenaContieneCadena cadena otraCadena)

cadenaContieneCadena :: String -> String -> Bool
cadenaContieneCadena cadena otraCadena = isPrefixOf cadena otraCadena

tieneMismaLongitud :: String -> String -> Bool
tieneMismaLongitud cadena otraCadena = (==) (length cadena) (length otraCadena)

esEnergizante :: Bebida -> Bool
esEnergizante (Cafe nombre _) = esLaMismaPalabra nombre "capuchino"
esEnergizante (Gaseosa sabor grAz) = esLaMismaPalabra sabor "pomelo" && grAz > 10

---------------------------------------------------------------------------------------------------------------

-- 3) Resolver la función find’ que encuentra el primer elemento que cumple una condición. No se puede resolver con recursividad. Si ningún elemento cumple la condición dejar que falle.


find' :: (a -> Bool) -> [a] -> a
find' funcion lista = (head.filter funcion) lista

---------------------------------------------------------------------------------------------------------------

-- 3.1) Aprovechar la función find’ para aplicarla a este dominio.

data Politico = Politico {proyectosPresentados :: [String], sueldo' :: Float, edad :: Int } deriving Show
 
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]

politicoJoven :: [Politico] -> Politico
politicoJoven listaPoliticos = elegirAlgunPoliticoSegun politicoEsJoven politicos

politicoEsJoven :: Politico -> Bool
politicoEsJoven (Politico _ _ edad) = edad < 50

presentoAlMenosTresProyectos :: [Politico] -> Politico
presentoAlMenosTresProyectos listaPoliticos = elegirAlgunPoliticoSegun (presentoMasProyectos 3) politicos

elegirAlgunPoliticoSegun :: (Politico -> Bool) -> [Politico] -> Politico
elegirAlgunPoliticoSegun criterio lista = find' criterio lista

presentoMasProyectos :: Int -> Politico -> Bool
presentoMasProyectos cantidad (Politico pp _ _) = (>) (length pp) cantidad