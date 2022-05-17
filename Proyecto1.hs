"MATERIA: Introducción a los algoritmos"
"AUTOR: Ana De Pascuale, mail: ana.de.pascuale@mi.unc.edu.ar, github/anadepascuale"

"PROYECTO 1, TRABAJAMOS EN HASKELL."

--1) a-b-c

esCero :: Int -> Bool
esCero x | x == 0 = True
         | x < 0 = False
         | x > 0 = False
         | otherwise = False

esPositivo :: Int -> Bool
esPositivo x | x >= 0 = True
             | x < 0 = False
             | otherwise = False

esVocal :: Char -> Bool
esVocal x | x == 'a' = True
          | x == 'e' = True
          | x == 'i' = True
          | x == 'o' = True
          | x == 'u' = True
          | otherwise = False

--2) a-b-c-d-e  

paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) | x == True && (paratodo xs) = True
                | otherwise = False

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + (sumatoria xs)  

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * (productoria xs)


factorial :: Int -> Int
factorial x | x == 0 = 1
            | x == 1 = 1
            | otherwise = x * (factorial x - 1)

promedio :: [Int] -> Int
promedio (xs) = (sumatoria xs) `div` (length xs)

--3)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n (x : xs) = (n == x) || pertenece n xs

--4) a-b-c-d

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) v = (v x) && paratodo' xs v

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = False
existe' (x:xs) t = t x || existe' xs t

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x:xs) f = f (x) + (sumatoria' xs f)
 

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) h = h (x) * productoria' xs h

--5)

paraTodo :: [Bool] -> Bool
paraTodo xs = paratodo' xs id 
 
 --PARA MI
--id :: a -> a
--id a = a 

--6) a-b-c-d-e

todosPares1 :: [Int] -> Bool
todosPares1 xs= paratodo' xs even

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo b xs= existe' xs (esMultiplo b)

esMultiplo :: Int-> Int -> Bool 
esMultiplo a b = mod a b == 0

factorial' :: Int -> Int
factorial' a = productoria' [0..a]  id

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n] cuadrado 

cuadrado :: Int -> Int
cuadrado a = a * a

soloPares :: Int -> Int
soloPares a | even a = a
            | otherwise = 1


multiplicaPares :: [Int] -> Int
multiplicaPares [] = 1
multiplicaPares xs = productoria' xs soloPares 

--7)
-- de la funcion map  lo que hace es tomar una funcion y una lista, aplica esa funcion a cada elemento que compone la lista,
-- la transforma en una nueva lista. En el caso de la funcion filter, es una funcion que toma un predicado y una lista y 
-- devuelve una lista con los elementos que satisfacen al predicado, filtra la lista anterior, para darte una nueva.
-- A Entiendo que la funcion map succ, nos pide que apliquemos succ a la lista, y nos entregue una nueva, satisfaciendo 
-- lo que necesitamos, en el caso del ejemplo de lista que nos dan: map succ [1, -4, 6, 2, -8], nos devolveria [2,-3,7,3,-7]
-- y equivale hacer map (+1) [1,-4,6,2,-8]
-- B  y la expresion filter esPositivo [1, -4, 6, 2, -8], aplica la funcion esPositivo sobre la lista, y la filtra. Nos 
-- devuelve una nueva lista que solo posee los positivos, en este caso seria [1,6,2]

--8) a-b

duplicando :: [Int] -> [Int]
duplicando [] = []
duplicando (x:xs) = x * 2 :(duplicando xs) 

-- si yo pongo en la terminal map (*2) y una lista cualquiera, va a cumplir la misma funcion que duplicando

duplicandoconmap :: [Int] -> [Int]
duplicandoconmap xs = map (*2) xs

-- Es parecida a la que hice para hacer Multiplica pares, pero con listas
--Ahora que aprendi a dejar comentarios, quiero hacerlo todo el tiempo. Perdon, cambio y fuera

--9) a-b

paresNadamas :: [Int] -> [Int]
paresNadamas [] = []
paresNadamas (x:xs) | even x = x:(paresNadamas xs)
                    | otherwise = (paresNadamas xs)

--usando la funcion filter

paresNamas :: [Int] -> [Int]
paresNamas xs = filter even xs

-- como mejoraria el 6e que era multiplicar pares ?

multiplicaPares1 :: [Int] -> Int
multiplicaPares1 xs = productoria (filter even xs)

--primero lo probe con productoria' y no funcionó, no se porque. Preguntar.

multiplicaPares' :: [Int] -> Int
multiplicaPares' xs = productoria' (filter even xs) id

--faltaba identidad!!!

--10) a)

primiIgualesA :: (Eq a) => a -> [a] -> [a]
primiIgualesA _ [] = []
primiIgualesA s (x:xs) | (s==x) = x : (primiIgualesA s xs)
                       | otherwise = []

--NOTACIONES PARA MI O A QUIEN LE INTERESE: OK este ejercicio me costó. la flecha del (Eq a) se hace con el = > pero junto. Cuando tenés un tipo a, 
--Haskell se asegura que todos los datatypes 
--sean compatibles con tu función
--Si vas a fijarte equivalencias, entonces van a haber datatypes que no soportan eso, entonces el programa va a fallar
--Por eso Haskell te dice que declarando a como un tipo Eq, solo los input que sean compatibles con equivalencia pueden ser usados en esa función
--Se declara así: función :: Eq a => a -> ... -> a OSEA cuando pones el Eq, significa que puede comparar que es lo que necesitamos.
-- Esto es todo lo que hay que saber de las subclases ya que simplemente son restriscciones de clase dentro de la definición de una clase.
--Ahora bien el takewhile : takeWhile p xs es el mayor prefijo de xs cuyos elementos satisfacen el predicado p
--takewhile, toma el tramo inicial de la lista desde el primer elemento. 

--10) b)

primiIgualesA' :: (Eq a) => a -> [a] -> [a]
primiIgualesA' s xs = takeWhile (==s) xs

-- como en el siguiente ejercicio compara entre los elementos de la lista, y no usa un elemento externo cambio el tipo, y tendria que hacer una versión 
--de primiIgualesA

--11) a)
primiIguales :: (Eq a) => [a] -> [a]
primiIguales [] = []
primiIguales [x] = [x]
primiIguales (x:(y:xs)) | (x==y) = x : (primiIguales (y:xs))
                        | otherwise = [x]

--Pensar en los casos, tipos de listas. El que me di cuenta al último fue la lista con un solo elemento.

--11)b) Esta versión tipa, pero habia leido mal la consigna para hacerla con el punto 10, pero no la quiero borrar.

primIguales' :: (Eq a) => [a] -> [a]
primIguales' [] = []
primIguales' xs = primiIguales xs 


primIguales :: (Eq a) => [a] -> [a]
primIguales [] = []
primIguales xs=  primiIgualesA' (head xs) xs

--12) 
--a) Esta bien formulada porque si bien el parámetro x puede interpretarse como un valor, y como no sabemos q valor representa x, perfectamente 
--puede ser del tipo (a,b), la subexpresión ya la dijimos antes, x es del tipo (a,b), y cubre todos los casos.
-- b) Esta bien tipada ya que el argumento responde al tipo de la funcion, que es de la clase de tipo (a,b) que es una 2-upla y en este caso (x,y) toman 
-- distintos valores que se corresponden a la forma de una tupla, que es su tamaño determinado, orden, etc. Y cubre todos los casos.
--c) Esta función no está bien tipada ya que el tipo pide una lista de 2-upla, y en la definición vemos que nos dan un par (a,b), deberia ser una lista.
--d) Está bien tipada, el tipo es de una lista de 2-uplas, y en la definición x representa a la tupla, pero no cubre todos los casos, porque falta el caso 
--base
--e) Esta bien tipada, el argumento se corresponde con la definición, y son de tipo tuplas, pueden haber dudas, pero en la definición  tenemos
-- f ((x, y) : ((a, b) : xs)) en donde todo pertenece a una lista, nuestro primer elemento (x,y) esta pegada a una lista  : ((a, b) : xs), y su vez
-- esa lista  posee una tupla pegada a una lista xs
-- f) Esta bien tipada, el parámetro y el argumento tienen la misma forma, (x,y) reprensetan a la tupla (a,b), pero no denotan todos los casos,
-- debería estar el caso de la lista con vacía, y la lista que posee solo uncon elemento. No olvidemos tambien que falta el caso en donde el primer 
--elemento de la tupla sea distinto de 0.
--g) Está mal tipada, porque a puede tomar distintos tipos y no se limita a un solo entero (1).
--h) Está bien tipada, se corresponde el parámetro con el argumento, es una lista de tuplas, compuesta del primer elemento que es un INT, y un tipo a.
--pegadas a la lista. En la definición vemos que no se cubren todos los casos, ya que como en el punto (f), faltarían  lista vacía, y el caso en donde el
--primer elemento de la lista es un entero diferente a 1.
--i) Esta bien tipada, poseemos una función de tipo Int, y un Int. Además se cubren todos los casos.
--j)Lo mismo que en el punto anterior, teniendo en cuenta que el int puede tomar cualquier valor ademas de 3, ese seria el único caso que faltaría.
--k)Esta mal tipada, la función (Int->Int) representa un solo valor, y en total tenemos dos parámtros pero en la definición nos dan 3 valores, no tiene 
--sentido, además no podrían limitarse a esos números solamente.
--l)Esta bien tipada, en el parametro nos dan un tipo a -> (a -> a), y tenemos dos parámetros, la función y el a. Y se cubren todos los casos

--13)
--a) f :: (a, b) -> b, el argumento sería f (m n) = n
--b) f :: (a, b) -> c, esta mal tipada, no puede dar un c nada que ver, si recibo algo del tipo a y del tipo b , tendria que devolver algo del tipo a ó b
--c) f :: a -> b, en este caso tambien esta mal tipada, ya que debería devolver algo del tipo a.
--d) f :: (a -> b) -> a -> b, el argumento sería f t a = t a 
--e) f :: (a -> b) -> [a] -> [b], el argumento sería, 
--f g [] = []
--f g (x:xs) = [g x] // (g x) : f g xs
--f) f :: (a -> b) -> a -> c, la definición de la función no podría darse porque no está bien tipada, no puede dar como resultado un tipo c que no tiene 
-- nada que ver con los tipos anteriores.
--g) f :: (a -> b) -> (b -> c) -> a -> c, la definición es f g t a = t (g a)