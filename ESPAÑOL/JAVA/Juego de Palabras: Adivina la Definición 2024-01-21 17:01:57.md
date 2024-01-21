```java
import java.util.Scanner;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

public class JuegoDePalabras {

    private static final List<String> PALABRAS = new ArrayList<>();
    private static final Map<String, String> DEFINICIONES = new HashMap<>();
    private static final Random RANDOM = new Random();

    public static void main(String[] args) {
        // Inicializar el juego con palabras y definiciones
        inicializarJuego();

        // Crear un escáner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Mostrar las instrucciones del juego
        System.out.println("Bienvenido al juego de palabras!");
        System.out.println("Instrucciones:");
        System.out.println("1. Se te dará una palabra.");
        System.out.println("2. Tu objetivo es adivinar la definición de la palabra.");
        System.out.println("3. Tienes tres intentos para adivinar la definición.");
        System.out.println("4. Si adivinas la definición, ganas un punto.");
        System.out.println("5. Si no adivinas la definición, pierdes un punto.");
        System.out.println("6. El juego termina cuando hayas adivinado o fallado todas las palabras.");

        // Iniciar el juego
        int puntos = 0;
        int palabrasIntentadas = 0;

        while (palabrasIntentadas < PALABRAS.size()) {
            // Obtener una palabra aleatoria de la lista de palabras
            String palabra = PALABRAS.get(RANDOM.nextInt(PALABRAS.size()));

            // Obtener la definición de la palabra
            String definicion = DEFINICIONES.get(palabra);

            // Mostrar la palabra al usuario
            System.out.println("Palabra: " + palabra);

            // Obtener la entrada del usuario
            System.out.println("¿Cuál es la definición de la palabra?");
            String respuesta = scanner.nextLine();

            // Comprobar si el usuario adivinó la definición
            boolean adivino = respuesta.equals(definicion);

            // Mostrar el resultado al usuario
            if (adivino) {
                System.out.println("¡Correcto! Has ganado un punto.");
                puntos++;
            } else {
                System.out.println("Incorrecto. Tienes dos intentos más.");

                // Obtener la entrada del usuario para el segundo intento
                System.out.println("¿Cuál es la definición de la palabra?");
                respuesta = scanner.nextLine();

                // Comprobar si el usuario adivinó la definición
                adivino = respuesta.equals(definicion);

                // Mostrar el resultado al usuario
                if (adivino) {
                    System.out.println("¡Correcto! Has ganado un punto.");
                    puntos++;
                } else {
                    System.out.println("Incorrecto. Tienes un intento más.");

                    // Obtener la entrada del usuario para el tercer intento
                    System.out.println("¿Cuál es la definición de la palabra?");
                    respuesta = scanner.nextLine();

                    // Comprobar si el usuario adivinó la definición
                    adivino = respuesta.equals(definicion);

                    // Mostrar el resultado al usuario
                    if (adivino) {
                        System.out.println("¡Correcto! Has ganado un punto.");
                        puntos++;
                    } else {
                        System.out.println("Incorrecto. Has perdido un punto.");
                        puntos--;
                    }
                }
            }

            // Incrementar el número de palabras intentadas
            palabrasIntentadas++;
        }

        // Mostrar el resultado del juego al usuario
        System.out.println("Fin del juego. Tu puntaje es: " + puntos);
    }

    private static void inicializarJuego() {
        PALABRAS.add("manzana");
        PALABRAS.add("naranja");
        PALABRAS.add("pera");
        PALABRAS.add("banana");
        PALABRAS.add("uva");

        DEFINICIONES.put("manzana", "Fruta roja o verde, dulce y crujiente.");
        DEFINICIONES.put("naranja", "Fruta cítrica, jugosa y ácida.");
        DEFINICIONES.put("pera", "Fruta verde, dulce y jugosa.");
        DEFINICIONES.put("banana", "Fruta tropical, amarilla y dulce.");
        DEFINICIONES.put("uva", "Fruta pequeña, morada o verde, que crece en racimos.");
    }
}
```

Explicación del código:

* El código comienza importando las bibliotecas necesarias para el juego.
* La clase `JuegoDePalabras` contiene el método `main()` que es el punto de entrada del programa.
* Se crea una lista de palabras (`PALABRAS`) y un mapa de definiciones (`DEFINICIONES`) para inicializar el juego.
* Se crea un objeto `Random` para generar números aleatorios.
* El juego muestra las instrucciones al usuario.
* El juego inicia un bucle que se ejecuta mientras el número de palabras intentadas sea menor que el número de palabras en la lista.
* Dentro del bucle, se obtiene una palabra aleatoria de la lista de palabras y se muestra al usuario.
* El usuario debe adivinar la definición de la palabra. Tiene tres intentos para hacerlo.
* Si el usuario adivina la definición, se le suma un punto. Si no la adivina, se le resta un punto.
* El número de palabras intentadas se incrementa en uno.
* El juego termina cuando el usuario ha adivinado o fallado todas las palabras.
* El juego muestra el resultado del juego al usuario.

El código está escrito en español y es fácil de entender. Utiliza bucles, condicionales y variables. El código también utiliza la biblioteca `Random` para generar números aleatorios.