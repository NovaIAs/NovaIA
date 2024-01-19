```haskell
-- Definir un módulo
module JuegoDeLaVida where

-- Importar la librería de listas
import Data.List

-- Definir el tipo de datos para representar una célula del juego
data Celula = Viva | Muerta deriving (Eq, Show)

-- Definir el tipo de datos para representar un tablero del juego
type Tablero = [[Celula]]

-- Función para crear un tablero vacío de un tamaño determinado
crearTablero :: Int -> Int -> Tablero
crearTablero filas columnas = replicate filas $ replicate columnas Muerta

-- Función para obtener las células vecinas de una célula dada en un tablero
vecinos :: Tablero -> Int -> Int -> [Celula]
vecinos tablero fila columna =
  [ tablero !! filaAnterior !! columnaAnterior,
    tablero !! filaAnterior !! columna,
    tablero !! filaAnterior !! columnaSiguiente,
    tablero !! fila !! columnaAnterior,
    tablero !! fila !! columnaSiguiente,
    tablero !! filaSiguiente !! columnaAnterior,
    tablero !! filaSiguiente !! columna,
    tablero !! filaSiguiente !! columnaSiguiente
  ]
  where
    filaAnterior = max 0 $ fila - 1
    columnaAnterior = max 0 $ columna - 1
    filaSiguiente = min (length tablero - 1) $ fila + 1
    columnaSiguiente = min (length (tablero !! fila) - 1) $ columna + 1

-- Función para aplicar las reglas del juego a una célula dada y sus vecinas
proximaGeneracion :: Celula -> [Celula] -> Celula
proximaGeneracion celula vecinos =
  case celula of
    Viva ->
      if length (filter (== Viva) vecinos) == 2 || length (filter (== Viva) vecinos) == 3 then Viva else Muerta
    Muerta ->
      if length (filter (== Viva) vecinos) == 3 then Viva else Muerta

-- Función para aplicar las reglas del juego a todas las células de un tablero
siguienteGeneracion :: Tablero -> Tablero
siguienteGeneracion tablero =
  map
    (map (proximaGeneracion) (vecinos tablero))
    [0 .. length tablero - 1]

-- Función para imprimir un tablero en la consola
imprimirTablero :: Tablero -> IO ()
imprimirTablero tablero =
  putStrLn $
    unlines
      [ intercalate " " (map (\celula -> show celula) fila)
      | fila <- tablero
      ]

-- Función principal del juego
main :: IO ()
main = do
  let tableroInicial = crearTablero 20 20
  imprimirTablero tableroInicial
  putStrLn ""
  imprimirTablero (siguienteGeneracion tableroInicial)
```

Explicación del código:

1. **Módulo y librerías:**
   - Se define un módulo llamado `JuegoDeLaVida`.
   - Se importa la librería `Data.List` para trabajar con listas.

2. **Tipos de datos:**
   - Se define el tipo de datos `Celula` para representar una célula del juego, que puede estar `Viva` o `Muerta`.
   - Se define el tipo de datos `Tablero` para representar un tablero del juego, que es una lista de listas de `Celula`.

3. **Funciones para crear y modificar tableros:**
   - `crearTablero` crea un tablero vacío de un tamaño determinado.
   - `vecinos` obtiene las células vecinas de una célula dada en un tablero.
   - `siguienteGeneracion` aplica las reglas del juego a una célula dada y sus vecinas para determinar su estado en la siguiente generación.
   - `imprimirTablero` imprime un tablero en la consola.

4. **Función principal:**
   - La función `main` es la función principal del juego.
   - Crea un tablero inicial vacío de tamaño 20x20.
   - Imprime el tablero inicial en la consola.
   - Imprime la siguiente generación del tablero en la consola.