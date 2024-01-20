Bien, aquí tienes un código complejo en Groovy que creará una aplicación de consola que te permitirá gestionar una lista de tareas:

```
// Importar las clases necesarias
import java.util.Scanner;
import java.io.File;
import java.io.PrintWriter;

// Clase principal de la aplicación
class GestorDeTareas {

    // Método main que se ejecuta al iniciar la aplicación
    static void main(String[] args) {
        // Crear un objeto Scanner para leer la entrada del usuario
        Scanner sc = new Scanner(System.in);

        // Crear un objeto File para almacenar la lista de tareas
        File listaDeTareas = new File("listaDeTareas.txt");

        // Crear un objeto PrintWriter para escribir en el archivo de lista de tareas
        PrintWriter escritor = new PrintWriter(listaDeTareas);

        // Mostrar menú de opciones
        while (true) {
            System.out.println("\n\n--- Gestor de Tareas ---\n");
            System.out.println("1. Agregar tarea");
            System.out.println("2. Mostrar lista de tareas");
            System.out.println("3. Marcar tarea como completada");
            System.out.println("4. Eliminar tarea");
            System.out.println("5. Salir");
            System.out.print("Elige una opción: ");

            // Leer la opción del usuario
            int opcion = sc.nextInt();

            // Procesar la opción elegida
            switch (opcion) {
                case 1: // Agregar tarea
                    System.out.print("Nombre de la tarea: ");
                    String nombreTarea = sc.nextLine();
                    escritor.println(nombreTarea);
                    escritor.flush();
                    System.out.println("Tarea agregada con éxito.");
                    break;
                case 2: // Mostrar lista de tareas
                    System.out.println("\nLista de tareas:");
                    Scanner lector = new Scanner(listaDeTareas);
                    while (lector.hasNextLine()) {
                        String tarea = lector.nextLine();
                        System.out.println(tarea);
                    }
                    lector.close();
                    break;
                case 3: // Marcar tarea como completada
                    System.out.print("Nombre de la tarea a marcar como completada: ");
                    String nombreTareaACompletar = sc.nextLine();
                    Scanner lector2 = new Scanner(listaDeTareas);
                    PrintWriter escritor2 = new PrintWriter("listaDeTareasTemporal.txt");
                    while (lector2.hasNextLine()) {
                        String tarea = lector2.nextLine();
                        if (!tarea.equals(nombreTareaACompletar)) {
                            escritor2.println(tarea);
                        }
                    }
                    lector2.close();
                    escritor2.flush();
                    escritor2.close();
                    listaDeTareas.delete();
                    new File("listaDeTareasTemporal.txt").renameTo(listaDeTareas);
                    System.out.println("Tarea marcada como completada con éxito.");
                    break;
                case 4: // Eliminar tarea
                    System.out.print("Nombre de la tarea a eliminar: ");
                    String nombreTareaAEliminar = sc.nextLine();
                    Scanner lector3 = new Scanner(listaDeTareas);
                    PrintWriter escritor3 = new PrintWriter("listaDeTareasTemporal.txt");
                    while (lector3.hasNextLine()) {
                        String tarea = lector3.nextLine();
                        if (!tarea.equals(nombreTareaAEliminar)) {
                            escritor3.println(tarea);
                        }
                    }
                    lector3.close();
                    escritor3.flush();
                    escritor3.close();
                    listaDeTareas.delete();
                    new File("listaDeTareasTemporal.txt").renameTo(listaDeTareas);
                    System.out.println("Tarea eliminada con éxito.");
                    break;
                case 5: // Salir
                    System.out.println("Saliendo de la aplicación...");
                    sc.close();
                    escritor.close();
                    System.exit(0);
                    break;
                default:
                    System.out.println("Opción no válida.");
            }
        }
    }
}
```

Explicación del código:

* La clase `GestorDeTareas` es la clase principal de la aplicación.
* El método `main` es el método de entrada de la aplicación.
* El código crea un objeto `Scanner` para leer la entrada del usuario.
* El código crea un objeto `File` para almacenar la lista de tareas.
* El código crea un objeto `PrintWriter` para escribir en el archivo de lista de tareas.
* El código muestra un menú de opciones al usuario.
* El código procesa la opción elegida por el usuario.
* El código utiliza un bucle `while` para seguir mostrando el menú hasta que el usuario elija la opción de salir.
* El código utiliza un `switch` para procesar la opción elegida por el usuario.
* El código utiliza métodos de la clase `File` para agregar, marcar como completada y eliminar tareas de la lista de tareas.
* El código utiliza métodos de la clase `Scanner` para leer la entrada del usuario.
* El código utiliza métodos de la clase `PrintWriter` para escribir en el archivo de lista de tareas.