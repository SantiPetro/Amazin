module Lib where
import Text.Show.Functions

-- Parcial Funcional
-------------
-- Parte A --
-------------
data Usuario = Usuario {
    nick              :: String,
    indiceDeFelicidad :: Int,
    librosAdquiridos  :: [Libro], 
    librosLeidos      :: [Libro] 
} deriving (Show)

data Libro = Libro {
    titulo            :: String,
    escritor          :: String,
    cantidadDePaginas :: Int,
    comoAfectaGenero  :: Reaccion
} deriving (Show)

fede :: Usuario
fede = Usuario "Fede" 99 [elPrincipito] []

elPrincipito :: Libro
elPrincipito = Libro "El Principito" "Antoine de Saint-Exupéry" 92 cienciaFiccion

-------------
-- Parte B --
-------------
type Reaccion = Usuario -> Usuario 

comediaDramatica :: Reaccion
comediaDramatica = comedia id

comediaAbsurda :: Reaccion
comediaAbsurda = comedia (+5)

comediaSatirica :: Reaccion
comediaSatirica = comedia (*2)

comediaDefault :: Reaccion
comediaDefault = comedia (+10)

comedia :: (Int -> Int) -> Reaccion
comedia f = mapFelicidad f

mapFelicidad :: (Int -> Int) -> Usuario -> Usuario
mapFelicidad f unUsuario = unUsuario { indiceDeFelicidad = f $ indiceDeFelicidad unUsuario }

cienciaFiccion :: Reaccion
cienciaFiccion = invertirNick

invertirNick :: Usuario -> Usuario
invertirNick unUsuario = unUsuario {nick = reverse (nick unUsuario)}

terror :: Reaccion
terror = regalarLibrosAdquiridos

regalarLibrosAdquiridos :: Usuario -> Usuario
regalarLibrosAdquiridos unUsuario = unUsuario { librosAdquiridos = [] }

-------------
-- Parte C --
-------------
leer :: Libro -> Usuario -> Usuario
leer unLibro = (comoAfectaGenero unLibro) . (agregarALeidos unLibro)

agregarALeidos :: Libro -> Usuario -> Usuario
agregarALeidos unLibro unUsuario = unUsuario { librosLeidos = unLibro : (librosLeidos unUsuario) }

ponerseAlDia :: Usuario -> Usuario
ponerseAlDia unUsuario = foldr leer unUsuario (librosNoLeidos unUsuario)

librosNoLeidos :: Usuario -> [Libro]
librosNoLeidos unUsuario = filter (noFueLeido unUsuario) (librosAdquiridos unUsuario)

noFueLeido :: Usuario -> Libro -> Bool
noFueLeido unUsuario unLibro = all (librosDiferentes unLibro) (librosLeidos unUsuario)

librosDiferentes :: Libro -> Libro -> Bool
librosDiferentes unLibro otroLibro = distintoTitulo unLibro otroLibro || disintoEscritor unLibro otroLibro

disintoEscritor :: Libro -> Libro -> Bool
disintoEscritor unLibro otroLibro = esDistinto escritor unLibro otroLibro

distintoTitulo :: Libro -> Libro -> Bool
distintoTitulo unLibro otroLibro = esDistinto titulo unLibro otroLibro

esDistinto :: (Libro -> String) -> Libro -> Libro -> Bool
esDistinto f unLibro otroLibro = f unLibro /= f otroLibro

esFanatica :: String -> Usuario -> Bool
esFanatica unEscritor unUsuario = all (esMismoEscritor unEscritor) (librosLeidos unUsuario)

esMismoEscritor :: String -> Libro -> Bool
esMismoEscritor unEscritor = (== unEscritor) . escritor

-- ¿Puede una persona ponerse al día si adquirió una cantidad infinita de libros? 
-- Una persona no podría ponerse al día si adquiere una lista infinita de libros ya que no se terminaria de leer.

-------------
-- Parte D --
-------------
clasificarLibro :: Libro -> Tipo
clasificarLibro unLibro
  | cantidadDePaginas unLibro < 100  = Cuento
  | cantidadDePaginas unLibro <= 200 = NovelaCorta
  | otherwise                        = Novela

data Tipo = Cuento | NovelaCorta | Novela deriving (Show, Eq)

titulosPorTipo :: Usuario -> Tipo -> [String]
titulosPorTipo unUsuario unTipo = map titulo . filter (esMismoTipo unTipo) $ librosAdquiridos unUsuario

esMismoTipo :: Tipo -> Libro -> Bool 
esMismoTipo unTipo = (== unTipo) . clasificarLibro
