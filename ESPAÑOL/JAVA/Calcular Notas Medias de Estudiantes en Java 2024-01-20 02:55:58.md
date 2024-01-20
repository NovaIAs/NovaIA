```java
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;

public class Main {

    public static void main(String[] args) {
        // Crea un objeto Scanner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Crea un ArrayList para almacenar los nombres de los estudiantes
        ArrayList<String> estudiantes = new ArrayList<>();

        // Crea un HashMap para almacenar las notas de los estudiantes
        HashMap<String, ArrayList<Integer>> notas = new HashMap<>();

        // Lee el número de estudiantes
        System.out.println("¿Cuántos estudiantes hay?");
        int numEstudiantes = scanner.nextInt();

        // Lee los nombres y las notas de los estudiantes
        for (int i = 0; i < numEstudiantes; i++) {
            // Lee el nombre del estudiante
            System.out.println("Nombre del estudiante " + (i + 1) + ": ");
            String nombre = scanner.next();

            // Crea una lista para almacenar las notas del estudiante
            ArrayList<Integer> notasEstudiante = new ArrayList<>();

            // Lee las notas del estudiante
            for (int j = 0; j < 3; j++) {
                System.out.println("Nota " + (j + 1) + " del estudiante " + nombre + ": ");
                int nota = scanner.nextInt();

                // Añade la nota a la lista de notas del estudiante
                notasEstudiante.add(nota);
            }

            // Añade el nombre del estudiante y su lista de notas al HashMap
            notas.put(nombre, notasEstudiante);
        }

        // Calcula la nota media de cada estudiante
        HashMap<String, Double> medias = new HashMap<>();
        for (String nombre : notas.keySet()) {
            // Obtiene la lista de notas del estudiante
            ArrayList<Integer> notasEstudiante = notas.get(nombre);

            // Calcula la nota media del estudiante
            double media = 0;
            for (int nota : notasEstudiante) {
                media += nota;
            }
            media /= notasEstudiante.size();

            // Añade el nombre del estudiante y su nota media al HashMap
            medias.put(nombre, media);
        }

        // Imprime la lista de estudiantes y sus notas medias
        System.out.println("Lista de estudiantes y sus notas medias:");
        for (String nombre : medias.keySet()) {
            double media = medias.get(nombre);
            System.out.println(nombre + ": " + media);
        }

        // Cierra el objeto Scanner
        scanner.close();
    }
}
```

Explicación del código:

* El código crea un objeto Scanner para leer la entrada del usuario.
* Crea un ArrayList para almacenar los nombres de los estudiantes y un HashMap para almacenar las notas de los estudiantes.
* Lee el número de estudiantes y luego lee los nombres y las notas de cada estudiante.
* Calcula la nota media de cada estudiante y la almacena en un HashMap.
* Finalmente, imprime la lista de estudiantes y sus notas medias.

Este código es complejo porque utiliza varias estructuras de datos (ArrayList, HashMap) y realiza cálculos (calcular la nota media de cada estudiante). También es diferenciado porque no es un código típico que se encuentre en un libro de texto o en un tutorial.