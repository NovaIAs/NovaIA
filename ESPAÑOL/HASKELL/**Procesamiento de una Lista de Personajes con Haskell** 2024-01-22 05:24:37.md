```haskell
-- Importar los módulos necesarios
import Control.Monad (when)
import Data.List (nub, sort)

-- Definir el tipo de dato Personaje
data Personaje = Personaje {
  nombre :: String,
  edad :: Int,
  habilidades :: [String]
} deriving (Show)

-- Crear una lista de personajes
personajes = [
  Personaje "Mario" 25 ["saltar", "correr", "lanzar fuego"],
  Personaje "Luigi" 23 ["saltar", "correr", "lanzar hielo"],
  Personaje "Peach" 22 ["saltar", "correr", "lanzar corazones"],
  Personaje "Bowser" 40 ["saltar", "correr", "lanzar bolas de fuego"],
  Personaje "Donkey Kong" 35 ["saltar", "correr", "lanzar barriles"]
]

-- Función para obtener los nombres de los personajes
obtenerNombres :: [Personaje] -> [String]
obtenerNombres = map nombre

-- Función para obtener la edad promedio de los personajes
obtenerEdadPromedio :: [Personaje] -> Double
obtenerEdadPromedio = (/ fromIntegral) . sum . map edad

-- Función para obtener las habilidades únicas de los personajes
obtenerHabilidadesUnicas :: [Personaje] -> [String]
obtenerHabilidadesUnicas = nub . concatMap habilidades

-- Función para obtener los personajes que pueden saltar
obtenerPersonajesQuePuedenSaltar :: [Personaje] -> [Personaje]
obtenerPersonajesQuePuedenSaltar = filter (\p -> "saltar" `elem` habilidades p)

-- Función para obtener los personajes que pueden lanzar fuego
obtenerPersonajesQuePuedenLanzarFuego :: [Personaje] -> [Personaje]
obtenerPersonajesQuePuedenLanzarFuego = filter (\p -> "lanzar fuego" `elem` habilidades p)

-- Función para imprimir los resultados
imprimirResultados :: [Personaje] -> IO ()
imprimirResultados = mapM_ print

-- Función principal
main :: IO ()
main = do
  -- Obtener los nombres de los personajes
  nombres <- obtenerNombres personajes

  -- Obtener la edad promedio de los personajes
  edadPromedio <- obtenerEdadPromedio personajes

  -- Obtener las habilidades únicas de los personajes
  habilidadesUnicas <- obtenerHabilidadesUnicas personajes

  -- Obtener los personajes que pueden saltar
  personajesQuePuedenSaltar <- obtenerPersonajesQuePuedenSaltar personajes

  -- Obtener los personajes que pueden lanzar fuego
  personajesQuePuedenLanzarFuego <- obtenerPersonajesQuePuedenLanzarFuego personajes

  -- Imprimir los resultados
  putStrLn "Nombres de los personajes:"
  imprimirResultados nombres

  putStrLn "Edad promedio de los personajes:"
  print edadPromedio

  putStrLn "Habilidades únicas de los personajes:"
  imprimirResultados habilidadesUnicas

  putStrLn "Personajes que pueden saltar:"
  imprimirResultados personajesQuePuedenSaltar

  putStrLn "Personajes que pueden lanzar fuego:"
  imprimirResultados personajesQuePuedenLanzarFuego
```

Explicación del código:

* **Definiciones de tipos y datos:**

    * `Personaje`: Representa a un personaje con nombre, edad y habilidades.

* **Lista de personajes:**

    * `personajes`: Contiene una lista de personajes.

* **Funciones:**

    * `obtenerNombres`: Obtiene los nombres de los personajes.
    * `obtenerEdadPromedio`: Obtiene la edad promedio de los personajes.
    * `obtenerHabilidadesUnicas`: Obtiene las habilidades únicas de los personajes.
    * `obtenerPersonajesQuePuedenSaltar`: Obtiene los personajes que pueden saltar.
    * `obtenerPersonajesQuePuedenLanzarFuego`: Obtiene los personajes que pueden lanzar fuego.

* **Función principal `main`:**

    * Llama a las funciones para obtener los resultados y los imprime en la consola.