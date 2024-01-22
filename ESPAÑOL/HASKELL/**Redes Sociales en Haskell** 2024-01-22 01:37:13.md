```haskell
import Data.List ( nub, sort )
import Data.Function ( on )
import Data.Maybe ( fromMaybe )

data Persona = Persona
  { nombre :: String
  , edad :: Int
  , amigos :: [Persona]
  }

data Relacion = Relacion
  { personas :: [Persona]
  , grado :: Int
  }

relaciones :: [Relacion]
relaciones = map (:[]) [1..]

interseccion :: [a] -> [a] -> [a]
interseccion xs ys = sort $ nub $ filter (`elem` ys) xs

interseccionPersonas :: Persona -> Persona -> [Persona]
interseccionPersonas x y =
  interseccion (amigos x) (amigos y)

gradoRelacion :: [Relacion] -> Persona -> Persona -> Int
gradoRelacion rs x y =
  fromMaybe (-1) $
    find ((== Just) . grado ) $
    filter
      ( (==) $ sort [x, y]
      ) rs

caminoMasCorto :: [Relacion] -> Persona -> Persona -> [Persona]
caminoMasCorto rs x y
  | x == y = [x]
  | gradoRelacion rs x y >= 0 = map head $ recorrido
  | otherwise = []
  where
    recorrido = encontrarCamino rs x y [] (-1)

encontrarCamino :: [Relacion] -> Persona -> Persona -> [Persona] -> Int -> [[Persona]]
encontrarCamino rs destino actual camino gradoActual
  | actual == destino = [camino]
  | gradoActual == (-1) = []
  | gradoActual <= gradoRelacion rs actual destino = []
  | otherwise =
    concatMap
      (encontrarCamino rs destino)
      (filter
        ( (/= Just)
            . (`elem` camino)
        ) $
        map
          ( \(Relacion personas _) -> snd $ splitAt 1 personas)
          $
          filter
            ( (`elem` actual)
              . fst
            ) rs
      )

amigosComunes :: [Relacion] -> Persona -> Persona -> Int
amigosComunes rs x y =
  length $
    interseccionPersonas x y

relacionesAmigosComunes :: [Relacion] -> Persona -> Int
relacionesAmigosComunes rs x =
  sum $ map (amigosComunes rs x) (amigos x)

personaMasPopular :: [Relacion] -> [Persona]
personaMasPopular rs =
  sort
    (nub $
      filter
        ( (==) (maximum frecuencias)
        ) frecuencias
    ) on (compare `on`snd )
  where
    frecuencias = map (\x -> (x, relacionesAmigosComunes rs x)) personas

personas :: [Persona]
personas = [
  Persona "Juan" 20 [Persona "María" 21 [Persona "Pedro" 22 [], Persona "Ana" 23 []], Persona "Luis" 24 []],
  Persona "María" 21 [Persona "Juan" 20 [Persona "Pedro" 22 [], Persona "Ana" 23 []], Persona "Luis" 24 [], Persona "Carlos" 25 []],
  Persona "Pedro" 22 [Persona "Juan" 20 [Persona "María" 21 [Persona "Pedro" 22 [], Persona "Ana" 23 []], Persona "Luis" 24 []], Persona "Ana" 23 []],
  Persona "Ana" 23 [Persona "Juan" 20 [Persona "María" 21 [Persona "Pedro" 22 [], Persona "Ana" 23 []], Persona "Luis" 24 []], Persona "Pedro" 22 []],
  Persona "Luis" 24 [Persona "Juan" 20 [Persona "María" 21 [Persona "Pedro" 22 [], Persona "Ana" 23 []], Persona "Luis" 24 []], Persona "María" 21 [], Persona "Carlos" 25 []],
  Persona "Carlos" 25 [Persona "María" 21 [Persona "Pedro" 22 [], Persona "Ana" 23 []], Persona "Luis" 24 []]
  ]

main :: IO ()
main = do
  putStrLn "Relaciones:"
  mapM_ print relaciones
  putStrLn
  putStrLn "Intersección de personas:"
  print $ interseccionPersonas (personas !! 0) (personas !! 1)
  putStrLn
  putStrLn "Grado de relación:"
  print $ gradoRelacion relaciones (personas !! 0) (personas !! 1)
  putStrLn
  putStrLn "Camino más corto:"
  print $ caminoMasCorto relaciones (personas !! 0) (personas !! 4)
  putStrLn
  putStrLn "Amigos comunes:"
  print $ amigosComunes relaciones (personas !! 0) (personas !! 1)
  putStrLn
  putStrLn "Relaciones de amigos comunes:"
  print $ relacionesAmigosComunes relaciones (personas !! 0)
  putStrLn
  putStrLn "Persona más popular:"
  mapM_ print $ personaMasPopular relaciones
```

Este complejo código de Haskell simula una red social con datos de personas y sus relaciones.

* `Persona` es un tipo de datos que representa individuos con campos como nombre, edad y amigos.
* `Relacion` es un tipo de datos que representa relaciones entre personas, con campos como las personas involucradas y el grado de relación.
* `relaciones` es una lista de relaciones que define la red social.
* Hay muchas funciones auxiliares que operan sobre estos tipos de datos, como `interseccionPersonas`, `gradoRelacion`, `caminoMasCorto`, `amigosComunes`, `relacionesAmigosComunes` y `personaMasPopular`.

La función `main` es el punto de entrada al programa, donde se definen y utilizan las funciones auxiliares para mostrar información sobre la red social.

Este código es un buen ejemplo de cómo Haskell se puede utilizar para modelar y analizar datos complejos.