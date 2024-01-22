```java
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;

public class CodigoComplejo {

    public static void main(String[] args) {
        // Creación de un escáner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Creación de una lista para almacenar los nombres de los estudiantes
        ArrayList<String> nombresEstudiantes = new ArrayList<>();

        // Creación de un mapa para almacenar las notas de los estudiantes
        HashMap<String, Double> notasEstudiantes = new HashMap<>();

        // Bucle para solicitar al usuario los nombres y notas de los estudiantes
        while (true) {
            // Solicitud del nombre del estudiante
            System.out.println("Ingrese el nombre del estudiante:");
            String nombreEstudiante = scanner.nextLine();

            // Solicitud de la nota del estudiante
            System.out.println("Ingrese la nota del estudiante:");
            double notaEstudiante = Double.parseDouble(scanner.nextLine());

            // Almacenamiento del nombre y la nota del estudiante en la lista y el mapa
            nombresEstudiantes.add(nombreEstudiante);
            notasEstudiantes.put(nombreEstudiante, notaEstudiante);

            // Consulta al usuario si desea continuar ingresando datos
            System.out.println("¿Desea continuar ingresando datos? (y/n)");
            String respuesta = scanner.nextLine();

            // Salida del bucle si el usuario ingresa "n"
            if (respuesta.equals("n")) {
                break;
            }
        }

        // Cálculo de la nota promedio de los estudiantes
        double notaPromedio = 0.0;
        for (Double nota : notasEstudiantes.values()) {
            notaPromedio += nota;
        }
        notaPromedio /= notasEstudiantes.size();

        // Impresión de los nombres y notas de los estudiantes
        System.out.println("Lista de estudiantes y sus notas:");
        for (int i = 0; i < nombresEstudiantes.size(); i++) {
            System.out.println(nombresEstudiantes.get(i) + ": " + notasEstudiantes.get(nombresEstudiantes.get(i)));
        }

        // Impresión de la nota promedio de los estudiantes
        System.out.println("Nota promedio:");
        System.out.println(notaPromedio);
    }
}
```

Explicación del código:

* Se crea un escáner para leer la entrada del usuario.
* Se crean una lista para almacenar los nombres de los estudiantes y un mapa para almacenar las notas de los estudiantes.
* Se utiliza un bucle para solicitar al usuario los nombres y notas de los estudiantes, y se almacenan en la lista y el mapa.
* Se consulta al usuario si desea continuar ingresando datos, y se sale del bucle si ingresa "n".
* Se calcula la nota promedio de los estudiantes.
* Se imprimen los nombres y notas de los estudiantes.
* Se imprime la nota promedio de los estudiantes.