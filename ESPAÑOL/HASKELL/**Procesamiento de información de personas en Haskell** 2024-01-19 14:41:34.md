```haskell

-- Definición de tipos:

type Nombre = String
type Edad = Int
type ListadoPersonas = [(Nombre, Edad)]

-- Funciones:

-- Función para obtener la edad promedio de una lista de personas:

edadPromedio :: ListadoPersonas -> Int
edadPromedio listadoPersonas = sum [edad | (nombre, edad) <- listadoPersonas] `div` length listadoPersonas

-- Función para obtener la persona más vieja de una lista de personas:

personaMasVieja :: ListadoPersonas -> (Nombre, Edad)
personaMasVieja listadoPersonas = maximumBy (comparing snd) listadoPersonas

-- Función para obtener la persona más joven de una lista de personas:

personaMasJoven :: ListadoPersonas -> (Nombre, Edad)
personaMasJoven listadoPersonas = minimumBy (comparing snd) listadoPersonas

-- Función para obtener la lista de personas mayores de edad:

mayoresDeEdad :: ListadoPersonas -> ListadoPersonas
mayoresDeEdad listadoPersonas = filter (\(nombre, edad) -> edad >= 18) listadoPersonas

-- Función para obtener la lista de personas menores de edad:

menoresDeEdad :: ListadoPersonas -> ListadoPersonas
menoresDeEdad listadoPersonas = filter (\(nombre, edad) -> edad < 18) listadoPersonas

-- Función principal:

main :: IO ()
main = do
  -- Crear una lista de personas:

  listadoPersonas <- readLn :: IO ListadoPersonas

  -- Imprimir la edad promedio de la lista de personas:

  putStrLn ("Edad promedio: " ++ show (edadPromedio listadoPersonas))

  -- Imprimir la persona más vieja y la persona más joven de la lista de personas:

  let (personaMasViejaNombre, personaMasViejaEdad) = personaMasVieja listadoPersonas
  let (personaMasJovenNombre, personaMasJovenEdad) = personaMasJoven listadoPersonas

  putStrLn ("Persona más vieja: " ++ personaMasViejaNombre ++ ", " ++ show personaMasViejaEdad)
  putStrLn ("Persona más joven: " ++ personaMasJovenNombre ++ ", " ++ show personaMasJovenEdad)

  -- Imprimir la lista de personas mayores de edad y la lista de personas menores de edad:

  putStrLn "Personas mayores de edad:"
  mapM_ print (mayoresDeEdad listadoPersonas)

  putStrLn "Personas menores de edad:"
  mapM_ print (menoresDeEdad listadoPersonas)

```

Explicación del código:

1. **Definición de tipos:**

   * `Nombre`: tipo de datos para los nombres de las personas.
   * `Edad`: tipo de datos para las edades de las personas.
   * `ListadoPersonas`: tipo de datos para las listas de personas.

2. **Funciones:**

   * `edadPromedio`: función que calcula la edad promedio de una lista de personas.
   * `personaMasVieja`: función que calcula la persona más vieja de una lista de personas.
   * `personaMasJoven`: función que calcula la persona más joven de una lista de personas.
   * `mayoresDeEdad`: función que filtra la lista de personas y devuelve solo las personas mayores de edad.
   * `menoresDeEdad`: función que filtra la lista de personas y devuelve solo las personas menores de edad.

3. **Función principal:**

   * `main`: función principal del programa.
   * `readLn :: IO ListadoPersonas`: función que lee una lista de personas desde la entrada estándar.
   * `putStrLn`: función que imprime una cadena en la salida estándar.
   * `mapM_ print`: función que aplica la función `print` a cada elemento de una lista y luego imprime el resultado.

El programa primero lee una lista de personas desde la entrada estándar y luego utiliza las funciones definidas anteriormente para calcular y mostrar la edad promedio de la lista de personas, la persona más vieja y la persona más joven de la lista de personas, y las listas de personas mayores de edad y menores de edad.