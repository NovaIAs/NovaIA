```java
import java.util.Scanner;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.IntStream;

public class Complejo {

    private static final int NUM_HILOS = 4; // Número de hilos a utilizar
    private static final int NUM_TAREAS = 20; // Número de tareas a realizar
    private static final int TIEMPO_ESPERA = 1000; // Tiempo en milisegundos que espera cada tarea

    public static void main(String[] args) {
        ExecutorService executor = Executors.newFixedThreadPool(NUM_HILOS); // Crea un pool de hilos
        Scanner scanner = new Scanner(System.in); // Crea un escáner para leer la entrada del usuario

        int opcion; // Almacena la opción elegida por el usuario
        do {
            // Muestra el menú y lee la opción elegida por el usuario
            System.out.println("Menú:");
            System.out.println("1. Ejecutar tareas en paralelo");
            System.out.println("2. Salir");
            opcion = scanner.nextInt();

            switch (opcion) {
                case 1:
                    // Crea una lista de tareas
                    Callable<Integer>[] tareas = new Callable[NUM_TAREAS];
                    for (int i = 0; i < NUM_TAREAS; i++) {
                        tareas[i] = new TareaCompleja(TIEMPO_ESPERA); // Crea una nueva tarea compleja
                    }

                    // Envía las tareas al pool de hilos y obtiene los resultados
                    try {
                        Future<Integer>[] resultados = executor.invokeAll(tareas); // Envía las tareas al pool de hilos
                        for (Future<Integer> resultado : resultados) {
                            System.out.println("Resultado de la tarea: " + resultado.get()); // Obtiene el resultado de la tarea
                        }
                    } catch (Exception e) {
                        e.printStackTrace(); // Imprime el error en la consola
                    }

                    break;
                case 2:
                    executor.shutdown(); // Apaga el pool de hilos
                    break;
                default:
                    System.out.println("Opción no válida");
            }
        } while (opcion != 2);

        scanner.close(); // Cierra el escáner
    }

    private static class TareaCompleja implements Callable<Integer> {

        private final int tiempoEspera; // Tiempo en milisegundos que espera la tarea

        public TareaCompleja(int tiempoEspera) {
            this.tiempoEspera = tiempoEspera;
        }

        @Override
        public Integer call() throws Exception {
            // Simula una tarea compleja que tarda un tiempo en ejecutarse
            int resultado = 0;
            for (int i = 0; i < tiempoEspera; i++) {
                resultado += i;
            }
            return resultado;
        }
    }
}
```

Explicación:

- Este código crea un programa Java que ejecuta tareas en paralelo utilizando un pool de hilos.
- El programa tiene un menú que permite al usuario elegir entre ejecutar las tareas en paralelo o salir del programa.
- Cuando el usuario elige ejecutar las tareas en paralelo, el programa crea una lista de tareas y las envía al pool de hilos.
- Las tareas son ejecutadas en paralelo por los hilos del pool de hilos y los resultados son mostrados en la consola.
- Cada tarea es implementada como una clase que implementa la interfaz `Callable`.
- La clase `TareaCompleja` implementa la interfaz `Callable` y simula una tarea compleja que tarda un tiempo en ejecutarse.
- El método `call()` de la clase `TareaCompleja` ejecuta la tarea y devuelve el resultado.
- El pool de hilos es creado utilizando la clase `Executors` y tiene un número de hilos fijo.
- El programa utiliza la clase `Future` para obtener los resultados de las tareas cuando éstas se han completado.