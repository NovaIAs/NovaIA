```groovy
// Importar las clases necesarias.
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

// Clase principal.
class MiProgramaGroovy {

  // Método principal.
  static void main(String[] args) {
    // Crear una lista de números enteros.
    List<Integer> numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Crear un mapa de números enteros a sus cuadrados.
    Map<Integer, Integer> cuadrados = numeros.stream()
      .collect(Collectors.toMap({ it -> it }, { it -> it * it }));

    // Recorrer el mapa e imprimir los resultados.
    println("Números enteros y sus cuadrados:");
    cuadrados.each({
      println("Número: ${it.key}, Cuadrado: ${it.value}")
    });

    // Crear una clase persona.
    class Persona {
      String nombre;
      int edad;
    }

    // Crear una lista de personas.
    List<Persona> personas = [
      new Persona(nombre: "Juan", edad: 20),
      new Persona(nombre: "María", edad: 25),
      new Persona(nombre: "José", edad: 30)
    ];

    // Crear un mapa de nombres de personas a sus edades.
    Map<String, Integer> edades = personas.stream()
      .collect(Collectors.toMap({ it.nombre }, { it.edad }));

    // Recorrer el mapa e imprimir los resultados.
    println("\nNombres de personas y sus edades:");
    edades.each({
      println("Nombre: ${it.key}, Edad: ${it.value}")
    });
  }
}
```

Explicación:

* El código importa las clases necesarias, incluyendo `java.util.List`, `java.util.Map`, `java.util.stream.Collectors`, y la clase `Persona`.
* El método principal `main` crea una lista de números enteros, un mapa de números enteros a sus cuadrados, y una lista de personas.
* Utiliza la expresión lambda `stream().collect(...)` para crear el mapa de cuadrados y el mapa de edades.
* El método `each()` se utiliza para recorrer los mapas e imprimir los resultados.
* La clase `Persona` se utiliza para representar a las personas en la lista.
* El método `Collectors.toMap` se utiliza para crear un mapa a partir de una lista de elementos, utilizando una expresión lambda para especificar las claves y los valores del mapa.