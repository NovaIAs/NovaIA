```haskell
-- Importación de bibliotecas
import Data.List (nub)
import Data.Maybe (fromJust, catMaybes)
import System.IO (readFile, hPutStrLn)

-- Definición de tipos
type Nombre = String
type Edad = Int
type Profesion = String
type Persona = (Nombre, Edad, Profesion)

-- Definición de funciones
-- Función para leer un archivo de texto y devolver una lista de líneas
leerArchivo :: String -> IO [String]
leerArchivo nombreArchivo = readFile nombreArchivo >>= return . lines

-- Función para convertir una línea de texto en una persona
lineaAPersona :: String -> Persona
lineaAPersona linea = (nombre, edad, profesion)
  where
    partes = words linea
    nombre = partes !! 0
    edad = read (partes !! 1) :: Edad
    profesion = partes !! 2

-- Función para encontrar las profesiones únicas de una lista de personas
profesionesUnicas :: [Persona] -> [Profesion]
profesionesUnicas personas = nub $ map tercero personas

-- Función para encontrar las personas de una edad determinada en una lista de personas
personasPorEdad :: Edad -> [Persona] -> [Persona]
personasPorEdad edad personas = filter ((== edad) . snd) personas

-- Función para encontrar la edad promedio de una lista de personas
edadPromedio :: [Persona] -> Double
edadPromedio personas = sum (map snd personas) / fromIntegral (length personas)

-- Función para encontrar la persona más joven de una lista de personas
personaMasJoven :: [Persona] -> Persona
personaMasJoven personas = minimumBy (compare `on` snd) personas

-- Función para encontrar la persona más vieja de una lista de personas
personaMasVieja :: [Persona] -> Persona
personaMasVieja personas = maximumBy (compare `on` snd) personas

-- Función para encontrar las profesiones más comunes de una lista de personas
profesionesMasComunes :: [Persona] -> [Profesion]
profesionesMasComunes personas = catMaybes $ mapMaybe profesionMasComun (profesionesUnicas personas)
  where
    profesionMasComun :: Profesion -> Maybe Profesion
    profesionMasComun profesion =
      if frecuencia profesion == maximo
        then Just profesion
        else Nothing
      where
        frecuencia = length $ filter ((== profesion) . tercero) personas
        maximo = maximum $ map frecuencia (profesionesUnicas personas)

-- Función principal
main :: IO ()
main = do
  -- Leer el archivo de texto
  personas <- leerArchivo "personas.txt" >>= return . map lineaAPersona

  -- Imprimir las profesiones únicas
  hPutStrLn "Profesiones únicas:"
  mapM_ hPutStrLn (profesionesUnicas personas)

  -- Imprimir las personas de 30 años
  hPutStrLn "Personas de 30 años:"
  mapM_ hPutStrLn (personasPorEdad 30 personas)

  -- Imprimir la edad promedio
  hPutStrLn ("Edad promedio: " ++ show (edadPromedio personas))

  -- Imprimir la persona más joven
  hPutStrLn ("Persona más joven: " ++ show (personaMasJoven personas))

  -- Imprimir la persona más vieja
  hPutStrLn ("Persona más vieja: " ++ show (personaMasVieja personas))

  -- Imprimir las profesiones más comunes
  hPutStrLn "Profesiones más comunes:"
  mapM_ hPutStrLn (profesionesMasComunes personas)
```

Explicación del código:

1. **Importación de bibliotecas:** Se importan las bibliotecas necesarias para el código.
2. **Definición de tipos:** Se definen los tipos `Nombre`, `Edad`, `Profesion` y `Persona` para representar los datos de las personas.
3. **Definición de funciones:** Se definen las funciones necesarias para manipular los datos de las personas. Estas funciones incluyen:
    * `leerArchivo`: Lee un archivo de texto y devuelve una lista de líneas.
    * `lineaAPersona`: Convierte una línea de texto en una persona.
    * `profesionesUnicas`: Encuentra las profesiones únicas de una lista de personas.
    * `personasPorEdad`: Encuentra las personas de una edad determinada en una lista de personas.
    * `edadPromedio`: Encuentra la edad promedio de una lista de personas.
    * `personaMasJoven`: Encuentra la persona más joven de una lista de personas.
    * `personaMasVieja`: Encuentra la persona más vieja de una lista de personas.
    * `profesionesMasComunes`: Encuentra las profesiones más comunes de una lista de personas.
4. **Función principal:** La función `main` es la función principal del programa. Esta función llama a las funciones necesarias para leer el archivo de texto, manipular los datos de las personas e imprimir los resultados.