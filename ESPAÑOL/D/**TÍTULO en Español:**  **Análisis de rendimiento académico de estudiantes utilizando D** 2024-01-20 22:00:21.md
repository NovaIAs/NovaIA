```d
import std.stdio, std.array, std.string, std.algorithm;

// Crear una estructura para representar a un estudiante
struct Estudiante {
    string nombre;
    string apellidos;
    int edad;
    float promedio;
    list<string> materias;
};

// Crear una función para comparar estudiantes por su promedio
auto compararPorPromedio(const Estudiante& a, const Estudiante& b) {
    return a.promedio > b.promedio;
}

// Crear una lista de estudiantes
list<Estudiante> estudiantes = {
    Estudiante{"Juan", "Pérez", 20, 8.5, {"Matemáticas", "Física", "Química"}},
    Estudiante{"María", "García", 19, 9.0, {"Lengua", "Literatura", "Historia"}},
    Estudiante{"Pedro", "López", 21, 7.5, {"Economía", "Administración", "Finanzas"}},
    Estudiante{"Ana", "Fernández", 18, 8.0, {"Biología", "Geología", "Ecología"}},
    Estudiante{"Luis", "Rodríguez", 22, 9.5, {"Informática", "Ingeniería", "Robótica"}},
};

// Ordenar la lista de estudiantes por su promedio en orden descendente
estudiantes.sort(compararPorPromedio);

// Iterar sobre la lista de estudiantes y mostrar su información
foreach (auto& estudiante : estudiantes) {
    writeln("{} {} - Edad: {} - Promedio: {} - Materias: {}", estudiante.nombre, estudiante.apellidos, estudiante.edad, estudiante.promedio,
            "[", join(",", estudiante.materias), "]");
}

// Calcular el promedio general de todos los estudiantes
float promedioGeneral = 0.0;
foreach (auto& estudiante : estudiantes) {
    promedioGeneral += estudiante.promedio;
}
promedioGeneral /= estudiantes.length;

// Mostrar el promedio general
writeln("Promedio general: {}", promedioGeneral);
```

**Explicación del código:**

1. **Estructura `Estudiante`:** Se define una estructura llamada `Estudiante` para representar la información de cada estudiante. Esta estructura tiene los siguientes campos:

    * `nombre`: Nombre del estudiante.
    * `apellidos`: Apellidos del estudiante.
    * `edad`: Edad del estudiante.
    * `promedio`: Promedio de calificaciones del estudiante.
    * `materias`: Lista de materias que cursa el estudiante.

2. **Función compararPorPromedio:** Se define una función llamada `compararPorPromedio` que toma dos objetos de tipo `Estudiante` y los compara en función de su promedio. La función devuelve `true` si el promedio del primer estudiante es mayor que el del segundo, y `false` en caso contrario.

3. **Lista de estudiantes:** Se crea una lista de objetos de tipo `Estudiante` llamada `estudiantes`, que contiene información sobre varios estudiantes.

4. **Ordenar la lista de estudiantes:** Se ordena la lista de estudiantes por su promedio en orden descendente utilizando el método `sort`. El método `sort` toma una función de comparación como argumento, y utiliza esta función para ordenar los elementos de la lista. En este caso, se utiliza la función `compararPorPromedio` para ordenar los estudiantes por su promedio.

5. **Iterar sobre la lista de estudiantes:** Se itera sobre la lista de estudiantes utilizando el bucle `foreach`. El bucle `foreach` itera sobre todos los elementos de la lista, y en cada iteración asigna el elemento actual a la variable `estudiante`.

6. **Mostrar la información de los estudiantes:** Dentro del bucle `foreach`, se utiliza el método `writeln` para mostrar la información de cada estudiante. El método `writeln` toma una cadena de formato como argumento, y utiliza esta cadena para formatear y mostrar los datos de la variable `estudiante`.

7. **Calcular el promedio general:** Se calcula el promedio general de todos los estudiantes sumando el promedio de cada estudiante y dividiendo la suma por el número de estudiantes.

8. **Mostrar el promedio general:** Se utiliza el método `writeln` para mostrar el promedio general. El método `writeln` toma una cadena de formato como argumento, y utiliza esta cadena para formatear y mostrar el valor de la variable `promedioGeneral`.