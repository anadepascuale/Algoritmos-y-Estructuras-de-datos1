"MATERIA: Introducción a los algoritmos"
"AUTOR: Ana De Pascuale, mail: ana.de.pascuale@mi.unc.edu.ar, github/anadepascuale"

"PROYECTO 2, TRABAJAMOS EN HASKELL."

--1) a)

data Carrera = Matematica | Fisica | Computacion | Astronomia

--1)b)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

--2)

data Carrera' = Matematica' | Fisica' | Computacion' | Astronomia' deriving (Eq, Show, Ord, Bounded, Enum)

--3)  a)

--Cargo y Area son tipos enumerados

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq , Show, Enum )
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq , Show, Enum )

--Persona es un tipo algebraico

data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera' Ingreso  deriving (Show)
            
--b)

--el tipo seria "Cargo -> Persona"

--c) 
                        
type Ingreso = Int

esDoccargo :: [Persona] -> Cargo -> Int
esDoccargo [] _ = 0
esDoccargo (Docente Titular : xs) Titular = 1 + (cuantos_doc xs Titular) 
esDoccargo (Docente Asociado : xs) Asociado = 1 + (cuantos_doc xs Asociado)
esDoccargo (Docente Adjunto : xs) Adjunto = 1 + (cuantos_doc xs Adjunto)
esDoccargo (Docente Asistente : xs) Asistente = 1 + (cuantos_doc xs Asistente)
esDoccargo (Docente Auxiliar : xs)  Auxiliar = 1 + (cuantos_doc xs Auxiliar)
esDoccargo (_:xs) c = cuantos_doc xs c  

--Funcion con Eq

cuantos_doc ::  [Persona] -> Cargo -> Int 
cuantos_doc [] _ = 0 
cuantos_doc ((Docente c1):xs) c2 | c1 == c2 = 1 + (cuantos_doc xs c2)
                          | otherwise = (cuantos_doc xs c2)
cuantos_doc (_:xs) c = (cuantos_doc xs c)
 

mismoCargo :: Persona -> Cargo -> Bool
mismoCargo ( Docente Titular) Titular = True
mismoCargo ( Docente Asociado) Asociado = True
mismoCargo ( Docente Adjunto) Adjunto = True
mismoCargo ( Docente Asistente) Asistente = True
mismoCargo ( Docente Auxiliar) Auxiliar = True
mismoCargo _ _ = False


 --d) 

cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' [] _ = 0
cuantos_doc' xs cargo = length (filter (elCargo cargo) xs) 

 
elCargo :: Cargo -> Persona -> Bool
elCargo  cargo (Docente n) = n == cargo
elCargo _ _ = False

--4)

dividir :: Int -> Int -> Maybe Int
dividir x 0 = Nothing
dividir x y = Just (x `div` y)

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (primero:xs) = Just primero

--otra version

primerElemento' :: [a] -> Maybe a
primerElemento' [] = Nothing
primerElemento' xs = Just (head xs)

--5) a)

data Cola = VaciaC | Encolada Persona Cola deriving (Show)

--1) -- probar como: atender VaciaC y atender Just (Encolada Decane (Encolada 
--(NoDocente Administrativa) VaciaC))
--hay que poner Encolada ( Titular) porque sino haskell te toma como q le pasas 
--dos argumentos de tipos distintos

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ segun) = Just segun 
 
--2)  -- lo probe con encolar ( Titular) (Encolada Decane (Encolada Decane (Encolada (NoDocente 
--Administrativa) VaciaC)))

encolar :: Persona -> Cola -> Cola
encolar a VaciaC = (Encolada a VaciaC)
encolar a (Encolada cola xs) = Encolada cola (encolar a xs)

--3) lo probe con : busca (Encolada Decane (Encolada (Docente Titular) VaciaC)) 
--Titular 
--busca (Encolada Decane (Encolada (Docente Titular) (Encolada (NoDocente Ensenanza) 
--VaciaC))) Titular
busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC _ = Nothing
busca (Encolada (Docente z) xs) x | mismoCargo' z x = Just (Docente z)
                                  | otherwise = busca xs x
busca (Encolada _ xs) x = busca xs x

mismoCargo' :: Cargo -> Cargo -> Bool
mismoCargo' Titular Titular = True
mismoCargo' Asociado Asociado = True
mismoCargo' Adjunto Adjunto = True
mismoCargo' Asistente Asistente = True
mismoCargo' Auxiliar Auxiliar = True
mismoCargo' _ _ = False


--b) ¿A que otro tipo se parece Cola? Cola se parece al tipo lista porque representa
-- muchos elementos 

--6)
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show)

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

--a) ¿Como se debe instanciar el tipo ListaAsoc para representar la informaci´on almacenada
--en una gu´ıa telef´onica?

type GuiaTelefonica = ListaAsoc Int String
 --b)

 --1) 1) la_long (Nodo "hola" "saludo" (Nodo "adios" "despedida" Vacia)).

la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b (xs)) = 1 + (la_long xs)

--2) la_concat (Nodo "hola" "saludo" (Nodo "adios" "despedida" Vacia)) 
-- (Nodo "mata" "domingo" Vacia)

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia xs = xs
la_concat (Nodo a b (xs)) ys = (Nodo a b ) (la_concat xs ys)

--3) (Nodo "hola" "saludo" (Nodo "adios" "despedida" Vacia))

la_pares :: ListaAsoc a b -> [(a,b)] 
la_pares Vacia = []
la_pares (Nodo a b (xs)) = [(a,b)] ++ (la_pares xs) 

--4) la_busca (Nodo "hola" "saludo" (Nodo "adios" "despedida" Vacia)) "hola"

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing
la_busca (Nodo a b xs) z | (a==z) = Just (b)
                         | otherwise = la_busca xs z


--5) la_borrar "hola" (Nodo "hola" "saludo" (Nodo "adios" "despedida" Vacia))

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia
la_borrar x (Nodo a b xs) | (x==a) = la_borrar x xs
                          | otherwise = Nodo a b (la_borrar x xs)                        

--EJERCICIO 7

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)


a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama x _ y) = 1 + a_long x + a_long y


a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama x _ y) = a_hojas x + a_hojas y 


a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama x a y) = Rama (a_inc x) (a + 1) (a_inc y)  


a_map :: (a -> b) -> Arbol a -> Arbol b
a_map _ Hoja = Hoja
a_map f (Rama x a y) = Rama (a_map f x) (f a) (a_map f y) 


a_inc' :: Num a => Arbol a -> Arbol a
a_inc' a = a_map (+1) a

