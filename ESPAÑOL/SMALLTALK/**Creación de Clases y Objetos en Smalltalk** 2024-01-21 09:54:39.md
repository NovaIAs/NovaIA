```smalltalk
// Clase Persona
Persona clase := Objeto nuevaClase: 'Persona'.

Persona instVar: [
    "nombre: String",
    "edad: Number",
    "sexo: Character"
].

Persona constructor: [unNombre unaEdad unSexo] := ([
    nombre => unNombre,
    edad => unaEdad,
    sexo => unSexo
]).

Persona metodo: [nombre] := ([
    ^nombre
]).

Persona metodo: [edad] := ([
    ^edad
]).

Persona metodo: [sexo] := ([
    ^sexo
]).

Persona metodo: [cambiarNombre: unNombre] := ([
    nombre => unNombre
]).

Persona metodo: [cambiarEdad: unaEdad] := ([
    edad => unaEdad
]).

Persona metodo: [cambiarSexo: unSexo] := ([
    sexo => unSexo
]).

// Clase Estudiante
Estudiante clase := Persona nuevaSubclase: 'Estudiante'.

Estudiante instVar: [
    "universidad: String",
    "carrera: String"
].

Estudiante constructor: [unNombre unaEdad unSexo unaUniversidad unaCarrera] := [
    super constructor: unNombre unaEdad unSexo,
    universidad => unaUniversidad,
    carrera => unaCarrera
].

Estudiante metodo: [universidad] := ([
    ^universidad
]).

Estudiante metodo: [carrera] := ([
    ^carrera
]).

Estudiante metodo: [cambiarUniversidad: unaUniversidad] := ([
    universidad => unaUniversidad
]).

Estudiante metodo: [cambiarCarrera: unaCarrera] := ([
    carrera => unaCarrera
]).

// Clase Profesor
Profesor clase := Persona nuevaSubclase: 'Profesor'.

Profesor instVar: [
    "materia: String",
    "sueldo: Number"
].

Profesor constructor: [unNombre unaEdad unSexo unaMateria unSueldo] := [
    super constructor: unNombre unaEdad unSexo,
    materia => unaMateria,
    sueldo => unSueldo
].

Profesor metodo: [materia] := ([
    ^materia
]).

Profesor metodo: [sueldo] := ([
    ^sueldo
]).

Profesor metodo: [cambiarMateria: unaMateria] := ([
    materia => unaMateria
]).

Profesor metodo: [cambiarSueldo: unSueldo] := ([
    sueldo => unSueldo
]).

// Crear objetos
persona1 := Persona nueva: 'Juan' 20 'M'.
estudiante1 := Estudiante nuevo: 'María' 18 'F' 'Universidad de Sevilla' 'Ingeniería Informática'.
profesor1 := Profesor nuevo: 'Antonio' 45 'M' 'Matemáticas' 2000.

// Mostrar información
persona1 nombre mostrar.
persona1 edad mostrar.
persona1 sexo mostrar.
persona1 cambiarNombre: 'Juan Carlos'.
persona1 nombre mostrar.

estudiante1 universidad mostrar.
estudiante1 carrera mostrar.
estudiante1 cambiarUniversidad: 'Universidad de Granada'.
estudiante1 universidad mostrar.

profesor1 materia mostrar.
profesor1 sueldo mostrar.
profesor1 cambiarSueldo: 2500.
profesor1 sueldo mostrar.
```

Explicación del código:

* **Clase Persona**: Define la clase `Persona` con tres variables de instancia: `nombre`, `edad` y `sexo`. También define métodos para acceder y modificar estas variables de instancia.
* **Clase Estudiante**: Define la clase `Estudiante` como una subclase de `Persona` y añade dos variables de instancia: `universidad` y `carrera`. También define métodos para acceder y modificar estas variables de instancia.
* **Clase Profesor**: Define la clase `Profesor` como una subclase de `Persona` y añade dos variables de instancia: `materia` y `sueldo`. También define métodos para acceder y modificar estas variables de instancia.
* **Crear objetos**: Se crean objetos de las clases `Persona`, `Estudiante` y `Profesor`.
* **Mostrar información**: Se muestran los valores de las variables de instancia de los objetos.
* **Cambiar información**: Se cambian los valores de las variables de instancia de los objetos.

Este código es un ejemplo de cómo crear clases y objetos en Smalltalk. También muestra cómo utilizar los métodos para acceder y modificar las variables de instancia de los objetos.