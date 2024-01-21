```haskell
-- Definición de los tipos de datos.

data Persona = Persona { nombre :: String, apellido :: String, edad :: Int } deriving (Show)

data Curso = Curso { nombre :: String, profesor :: String, alumnos :: [Persona] } deriving (Show)

-- Función para crear una nueva persona.

crearPersona :: String -> String -> Int -> Persona
crearPersona nombre apellido edad = Persona nombre apellido edad

-- Función para crear un nuevo curso.

crearCurso :: String -> String -> [Persona] -> Curso
crearCurso nombre profesor alumnos = Curso nombre profesor alumnos

-- Función para añadir un alumno a un curso.

añadirAlumno :: Persona -> Curso -> Curso
añadirAlumno alumno curso = Curso (nombre curso) (profesor curso) (alumnos curso ++ [alumno])

-- Función para eliminar un alumno de un curso.

eliminarAlumno :: Persona -> Curso -> Curso
eliminarAlumno alumno curso = Curso (nombre curso) (profesor curso) (filter (\x -> x /= alumno) (alumnos curso))

-- Función principal.

main :: IO ()
main = do
  -- Se crean algunas personas.

  persona1 <- crearPersona "Juan" "García" 20
  persona2 <- crearPersona "Ana" "López" 22
  persona3 <- crearPersona "Pedro" "Sánchez" 24

  -- Se crea un nuevo curso.

  curso1 <- crearCurso "Matemáticas" "Profesor Pérez" [persona1, persona2]

  -- Se añade un alumno al curso.

  curso2 <- añadirAlumno persona3 curso1

  -- Se elimina un alumno del curso.

  curso3 <- eliminarAlumno persona2 curso2

  -- Se imprime la información del curso.

  print curso3
```

Explicación del código:

* El código define los tipos de datos `Persona` y `Curso` utilizando la sintaxis de registros de Haskell.
* La función `crearPersona` crea una nueva persona a partir de su nombre, apellido y edad.
* La función `crearCurso` crea un nuevo curso a partir de su nombre, profesor y alumnos.
* La función `añadirAlumno` añade un alumno a un curso.
* La función `eliminarAlumno` elimina un alumno de un curso.
* La función `main` es la función principal del programa. En ella se crean algunas personas, se crea un nuevo curso, se añade un alumno al curso y se elimina un alumno del curso. Finalmente, se imprime la información del curso.