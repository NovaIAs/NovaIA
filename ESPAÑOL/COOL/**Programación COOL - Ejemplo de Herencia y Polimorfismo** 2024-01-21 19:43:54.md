```cool
clase {
    clase_base {
        atributos:
            nombre: String
            edad: Integer

        métodos:
            inicializar(nombre: String, edad: Integer) {
                nombre := nombre
                edad := edad
            }

            es_mayor_de_edad() -> Boolean {
                edad >= 18
            }

            toString() -> String {
                "<ClaseBase: nombre=" + nombre + ", edad=" + edad + ">"
            }
    }

    clase_derivada extends clase_base {
        atributos:
            curso: String
            calificaciones: List

        métodos:
            inicializar(nombre: String, edad: Integer, curso: String, calificaciones: List) {
                superinicializar(nombre, edad)
                curso := curso
                calificaciones := calificaciones
            }

            get_promedio() -> Float {
                var suma = 0
                for calificacion in calificaciones {
                    suma += calificacion
                }

                return suma / calificaciones.size()
            }

            toString() -> String {
                "<ClaseDerivada: nombre=" + nombre + ", edad=" + edad + ", curso=" + curso + ", calificaciones=" + calificaciones + ">"
            }
    }

    main() {
        var estudiantes = List[ClaseDerivada]

        var estudiante1 = ClaseDerivada {
            nombre := "Juan"
            edad := 20
            curso := "Introducción a la Programación"
            calificaciones := List[Float] { 10, 9, 8, 7 }
        }

        var estudiante2 = ClaseDerivada {
            nombre := "María"
            edad := 25
            curso := "Análisis y Diseño de Sistemas"
            calificaciones := List[Float] { 9, 8, 7, 6 }
        }

        estudiantes.add(estudiante1)
        estudiantes.add(estudiante2)

        for estudiante in estudiantes {
            println(estudiante.toString())
            println("Promedio: " + estudiante.get_promedio())
        }
    }
}
```

Explicación del código:

* La clase `ClaseBase` define los atributos y métodos comunes a todas las clases que hereden de ella. En este caso, define los atributos `nombre` y `edad`, y los métodos `inicializar()` (que inicializa los atributos), `es_mayor_de_edad()` (que devuelve si la edad es mayor o igual a 18) y `toString()` (que devuelve una representación en cadena de la clase).

* La clase `ClaseDerivada` hereda de la clase `ClaseBase`. Esto significa que puede utilizar todos los atributos y métodos de la clase `ClaseBase`, además de poder definir sus propios atributos y métodos. En este caso, la clase `ClaseDerivada` define los atributos `curso` y `calificaciones`, y el método `get_promedio()` (que devuelve el promedio de las calificaciones).

* El método `main()` es el punto de entrada del programa. En este método se crea una lista de estudiantes de tipo `ClaseDerivada` y se añaden a la lista los estudiantes Juan y María. A continuación, se imprime la información de cada estudiante, incluyendo el promedio de sus calificaciones.

Este código es un ejemplo de un programa más complejo en COOL. Utiliza herencia, polimorfismo y listas, y muestra cómo se pueden utilizar estas características para crear programas más estructurados y flexibles.