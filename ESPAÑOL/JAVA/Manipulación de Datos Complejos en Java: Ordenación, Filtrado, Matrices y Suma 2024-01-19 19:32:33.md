```java
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class CodigoComplejo {

    public static void main(String[] args) {
        // Creamos una lista de objetos Persona
        List<Persona> personas = new ArrayList<>();
        personas.add(new Persona("Juan", "García", 25));
        personas.add(new Persona("María", "López", 30));
        personas.add(new Persona("Pedro", "Sánchez", 35));
        personas.add(new Persona("Ana", "Fernández", 40));

        // Ordenamos la lista de personas por nombre
        personas.sort(Comparator.comparing(Persona::getNombre));

        // Imprimimos la lista de personas ordenada
        System.out.println("Lista de personas ordenada por nombre:");
        for (Persona persona : personas) {
            System.out.println(persona);
        }

        // Filtramos la lista de personas para obtener solo las personas mayores de 30 años
        List<Persona> personasMayoresDe30 = personas.stream()
                .filter(persona -> persona.getEdad() > 30)
                .collect(Collectors.toList());

        // Imprimimos la lista de personas mayores de 30 años
        System.out.println("Lista de personas mayores de 30 años:");
        for (Persona persona : personasMayoresDe30) {
            System.out.println(persona);
        }

        // Creamos una matriz de números
        int[][] numeros = {
                {1, 2, 3},
                {4, 5, 6},
                {7, 8, 9}
        };

        // Imprimimos la matriz de números
        System.out.println("Matriz de números:");
        for (int[] fila : numeros) {
            for (int numero : fila) {
                System.out.print(numero + " ");
            }
            System.out.println();
        }

        // Calculamos la suma de todos los números de la matriz
        int sumaNumeros = Arrays.stream(numeros)
                .flatMapToInt(Arrays::stream)
                .sum();

        // Imprimimos la suma de todos los números de la matriz
        System.out.println("Suma de todos los números de la matriz:");
        System.out.println(sumaNumeros);
    }

    public static class Persona {

        private String nombre;
        private String apellido;
        private int edad;

        public Persona(String nombre, String apellido, int edad) {
            this.nombre = nombre;
            this.apellido = apellido;
            this.edad = edad;
        }

        public String getNombre() {
            return nombre;
        }

        public String getApellido() {
            return apellido;
        }

        public int getEdad() {
            return edad;
        }

        @Override
        public String toString() {
            return nombre + " " + apellido + " (" + edad + " años)";
        }
    }
}
```

Este código realiza varias operaciones complejas en Java, incluyendo:

* Creación de una lista de objetos Persona con nombre, apellido y edad.
* Ordenación de la lista de personas por nombre.
* Filtrado de la lista de personas para obtener solo las personas mayores de 30 años.
* Creación de una matriz de números.
* Impresión de la matriz de números.
* Cálculo de la suma de todos los números de la matriz.

El código también define una clase Persona con los campos nombre, apellido y edad, y los métodos getNombre(), getApellido() y getEdad() para acceder a dichos campos.

Este código demuestra el uso de varias características avanzadas de Java, como la programación funcional y el manejo de matrices.