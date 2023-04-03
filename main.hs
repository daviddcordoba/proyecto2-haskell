--1)a)

data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show, Eq)
--b)
titulo:: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Astronomia = "Licenciatura en Astronomia"
titulo Computacion = "Licenciatura en Computacion"
--c)
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving ( Eq,Ord, Bounded,Show)

cifradoAmericano:: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

--3)
--a)
minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = x `min`  (minimoElemento xs)

--b)
minimoElemento' :: (Ord a, Bounded a) => [a] -> a {- preguntar -}
minimoElemento' [] = minBound
minimoElemento' [x] = x
minimoElemento' (x:xs) = min x (minimoElemento' xs)

--c)
--minimoElemento' [Fa, La, Sol, Re, Fa] = Re

--4)
type Ingreso = Int
-- Tipos enumerados -> Tienen muchos constructores pero sin argumentos
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq,Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq,Show)

-- Tipos algebraicos que reciben argumentos
data Persona = Decano
            | Docente Cargo
            | NoDocente Area
            | Estudiante Carrera Ingreso deriving (Eq,Show)

--b) ¿Cual es el tipo del constructor Docente? -> El tipo de Docente es Cargo

--c)

cuantos_doc :: [Persona] -> Cargo -> Int {- El tipo Persona tiene un constructor que es 'Docente Cargo' -> :type Docente 'Cargo' = Persona-}
cuantos_doc xs c = length(filter (== Docente c) xs)

{- >> cuantos_doc [Docente Titular, Docente Titular] Titular = 2 -}

--d) La funcion si usa filter

--5)

data Alteracion = Bemol | Sostenido | Natural
data NotaMusical = Nota NotaBasica Alteracion 
{- data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si -}

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

{- sonidoCromatico recibe de argumento de tipo 'NotaMusical' que se construye a partir del Contructor 'Nota' que recibe dos argumentos para que se forme el tipo Nota musical, osea que yo voy a recibir algo asi : Nota 'notabasica: Re' 'Alteracion:Bemol' y ahi tengo algo del tipo NotaMusical -}

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota notaBasica alteracion) = case alteracion of
    Sostenido -> (sonido notaBasica + 1) -- sonido 'notaBasica' devuelve un numero
    Bemol -> (sonido notaBasica - 1)
    Natural -> sonido notaBasica

--c) Incluı el tipo NotaMusical a la clase Eq de manera tal que dos notas que tengan el mismo valor de sonidoCromatico se consideren iguales.
instance Eq NotaMusical where
    (Nota _notabasica1 _alteracion2) == (Nota _notabasica3 _alteracion4) = sonidoCromatico (Nota _notabasica1 _alteracion2) == sonidoCromatico (Nota _notabasica3 _alteracion4)

{- d
Inclu ́ı el tipo NotaMusical a la clase Ord definiendo el operador <=. Se debe definir
que una nota es menor o igual a otra si y s ́olo si el valor de sonidoCromatico para la
primera es menor o igual al valor de sonidoCromatico para la segunda.
-}
instance Ord NotaMusical where
    (Nota _notabasica1 _alteracion2) <= (Nota _notabasica3 _alteracion4) = sonidoCromatico (Nota _notabasica1 _alteracion2) <= sonidoCromatico (Nota _notabasica3 _alteracion4)

--6)
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento xs = Just (xs!!0)

--7)Tipos Recursivos = aparece como parametro en uno de sus contructores
{- data Persona = Decano
            | Docente Cargo
            | NoDocente Area
            | Estudiante Carrera Ingreso deriving (Eq,Show) -}
{- 
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq,Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq,Show)
 -}
data Cola = VaciaC | Encolada Persona Cola deriving (Show)

--a)
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _persona _cola) = Just _cola 
{- Aca -}

encolar :: Persona -> Cola -> Cola
encolar _persona VaciaC = Encolada _persona VaciaC
encolar _persona (Encolada _persona1 _cola) = Encolada _persona1 (encolar _persona _cola)

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC _ = Nothing
busca (Encolada _persona _cola) cargo | _persona == Docente cargo = Just _persona
                                      | otherwise = busca _cola cargo
--Aca pongo _cola porque en la primera cola que se fija es en 'Encolada _persona _cola' entonces se fija en la primer persona que es _persona y si no encuentra va a fijarse en la otra _cola = Encolada _persona2 _colaSiguiente
