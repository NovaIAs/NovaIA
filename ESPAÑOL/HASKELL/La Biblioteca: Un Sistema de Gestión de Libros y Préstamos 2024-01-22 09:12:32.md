```haskell
-- Módulo principal de La Biblioteca
module LaBiblioteca where

-- Importar los módulos necesarios
import Data.List (sort, nub)
import Data.Map (Map, fromList, toList, insert, lookup, delete)
import Control.Monad (forM_, mapM_, forM, ap)

-- Definir el tipo de dato Libro
data Libro = Libro {
    titulo :: String,
    autor :: String,
    isbn :: String,
    precio :: Double
} deriving (Show, Eq, Ord)

-- Definir el tipo de dato Biblioteca
data Biblioteca = Biblioteca {
    libros :: Map String Libro
} deriving (Show)

-- Crear una biblioteca vacía
bibliotecaVacia :: Biblioteca
bibliotecaVacia = Biblioteca { libros = fromList [] }

-- Añadir un libro a la biblioteca
añadirLibro :: Libro -> Biblioteca -> Biblioteca
añadirLibro libro biblioteca = Biblioteca { libros = insert (isbn libro) libro (libros biblioteca) }

-- Eliminar un libro de la biblioteca
eliminarLibro :: String -> Biblioteca -> Biblioteca
eliminarLibro isbn biblioteca = Biblioteca { libros = delete isbn (libros biblioteca) }

-- Buscar un libro en la biblioteca
buscarLibro :: String -> Biblioteca -> Maybe Libro
buscarLibro isbn biblioteca = lookup isbn (libros biblioteca)

-- Listar los libros de la biblioteca ordenados por título
listarLibros :: Biblioteca -> [Libro]
listarLibros biblioteca = sort $ toList (libros biblioteca)

-- Listar los libros de un autor específico
listarLibrosDeAutor :: String -> Biblioteca -> [Libro]
listarLibrosDeAutor autor biblioteca =
    let librosDeAutor = filter ((== autor) . autor) $ toList (libros biblioteca)
    in sort librosDeAutor

-- Calcular el precio total de los libros de la biblioteca
precioTotal :: Biblioteca -> Double
precioTotal biblioteca = sum $ map precio $ toList (libros biblioteca)

-- Definir el tipo de dato Préstamo
data Préstamo = Préstamo {
    libro :: Libro,
    usuario :: String,
    fechaPréstamo :: String,
    fechaDevolución :: String
} deriving (Show)

-- Definir el tipo de dato GestorPréstamos
data GestorPréstamos = GestorPréstamos {
    biblioteca :: Biblioteca,
    préstamos :: Map String Préstamo
} deriving (Show)

-- Crear un gestor de préstamos vacío
gestorPréstamosVacio :: GestorPréstamos
gestorPréstamosVacio = GestorPréstamos { biblioteca = bibliotecaVacia, préstamos = fromList [] }

-- Añadir un préstamo al gestor de préstamos
añadirPréstamo :: Préstamo -> GestorPréstamos -> GestorPréstamos
añadirPréstamo préstamo gestorPréstamos =
    GestorPréstamos {
        biblioteca = biblioteca gestorPréstamos,
        préstamos = insert (isbn (libro préstamo)) préstamo (préstamos gestorPréstamos)
    }

-- Eliminar un préstamo del gestor de préstamos
eliminarPréstamo :: String -> GestorPréstamos -> GestorPréstamo
eliminarPréstamo isbn gestorPréstamos =
    GestorPréstamos { 
        biblioteca = biblioteca gestorPréstamos,
        préstamos = delete isbn (préstamos gestorPréstamos)
    }

-- Buscar un préstamo en el gestor de préstamos
buscarPréstamo :: String -> GestorPréstamos -> Maybe Préstamo
buscarPréstamo isbn gestorPréstamos = lookup isbn (préstamos gestorPréstamos)

-- Listar los préstamos del gestor de préstamos ordenados por fecha de préstamo
litarPréstamos :: GestorPréstamos -> [Préstamo]
listarPréstamos gestorPréstamos = sort $ toList (préstamos gestorPréstamos)

-- Listar los préstamos de un usuario específico
listarPréstamosDeUsuario :: String -> GestorPréstamos -> [Préstamo]
listarPréstamosDeUsuario usuario gestorPréstamos =
    let préstamosDeUsuario = filter ((== usuario) . usuario) $ toList (préstamos gestorPréstamos)
    in sort préstamosDeUsuario

-- Calcular el número total de préstamos del gestor de préstamos
numeroTotalDePréstamos :: GestorPréstamos -> Int
numeroTotalDePréstamos gestorPréstamos = length $ toList (préstamos gestorPréstamos)

-- Calcular el número total de préstamos de un usuario específico
numeroTotalDePréstamosDeUsuario :: String -> GestorPréstamos -> Int
numeroTotalDePréstamosDeUsuario usuario gestorPréstamos =
    let préstamosDeUsuario = listarPréstamosDeUsuario usuario gestorPréstamos
    in length préstamosDeUsuario

-- Calcular el precio total de los préstamos de un usuario específico
precioTotalDePréstamosDeUsuario :: String -> GestorPréstamos -> Double
precioTotalDePréstamosDeUsuario usuario gestorPréstamos =
    let préstamosDeUsuario = listarPréstamosDeUsuario usuario gestorPréstamos
    in sum $ map (precio . libro) préstamosDeUsuario

-- Calcular el precio total de los préstamos del gestor de préstamos
precioTotalDePréstamos :: GestorPréstamos -> Double
precioTotalDePréstamos gestorPréstamos =
    let préstamos = toList (préstamos gestorPréstamos)
    in sum $ map (precio . libro) préstamos

-- Definir el tipo de dato Usuario
data Usuario = Usuario {
    nombre :: String,
    apellidos :: String,
    dirección :: String,
    email :: String
} deriving (Show, Eq, Ord)

-- Definir el tipo de dato GestorUsuarios
data GestorUsuarios = GestorUsuarios {
    usuarios :: Map String Usuario
} deriving (Show)

-- Crear un gestor de usuarios vacío
gestorUsuariosVacio :: GestorUsuarios
gestorUsuariosVacio = GestorUsuarios { usuarios = fromList [] }

-- Añadir un usuario al gestor de usuarios
añadirUsuario :: Usuario -> GestorUsuarios -> GestorUsuarios
añadirUsuario usuario gestorUsuarios =
    GestorUsuarios { usuarios = insert (email usuario) usuario (usuarios gestorUsuarios) }

-- Eliminar un usuario del gestor de usuarios
eliminarUsuario :: String -> GestorUsuarios -> GestorUsuarios
eliminarUsuario email gestorUsuarios =
    GestorUsuarios { usuarios = delete email (usuarios gestorUsuarios) }

-- Buscar un usuario en el gestor de usuarios
buscarUsuario :: String -> GestorUsuarios -> Maybe Usuario
buscarUsuario email gestorUsuarios = lookup email (usuarios gestorUsuarios)

-- Listar los usuarios del gestor de usuarios ordenados por nombre
listarUsuarios :: GestorUsuarios -> [Usuario]
listarUsuarios gestorUsuarios = sort $ toList (usuarios gestorUsuarios)

-- Calcular el número total de usuarios del gestor de usuarios
numeroTotalDeUsuarios :: GestorUsuarios -> Int
numeroTotalDeUsuarios gestorUsuarios = length $ toList (usuarios gestorUsuarios)

-- Crear una función auxiliar para convertir una lista de cadenas en una lista de libros
listasDeCadenasAListaDeLibros :: [String] -> [Libro]
listasDeCadenasAListaDeLibros [] = []