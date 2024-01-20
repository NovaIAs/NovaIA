```java
// Importamos las librerías necesarias
import java.util.Scanner;
import java.util.Random;

// Creamos una clase que representa un juego de adivinanzas
public class Adivinanzas {

    // Atributos de la clase
    private static final int NUMERO_MAXIMO = 100;
    private static final int NUMERO_MINIMO = 1;
    private static int numeroSecreto;
    private static int intentos = 0;

    // Método principal
    public static void main(String[] args) {
        // Creamos un objeto de la clase Scanner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Creamos un objeto de la clase Random para generar números aleatorios
        Random random = new Random();

        // Generamos un número secreto aleatorio entre 1 y 100
        numeroSecreto = random.nextInt(NUMERO_MAXIMO - NUMERO_MINIMO + 1) + NUMERO_MINIMO;

        // Damos la bienvenida al usuario y le explicamos el juego
        System.out.println("Bienvenido al juego de adivinanzas!");
        System.out.println("He generado un número secreto entre 1 y 100. Intenta adivinarlo!");

        // Bucle principal del juego
        while (true) {
            // Pedimos al usuario que introduzca un número
            System.out.print("Introduce un número: ");
            int numero = scanner.nextInt();

            // Incrementamos el número de intentos
            intentos++;

            // Comprobamos si el número introducido por el usuario es igual al número secreto
            if (numero == numeroSecreto) {
                // Si el número es igual, felicitamos al usuario y salimos del bucle
                System.out.println("¡Felicidades! Has acertado el número secreto en " + intentos + " intentos.");
                break;
            } else if (numero < numeroSecreto) {
                // Si el número es menor que el número secreto, decimos que el número secreto es mayor
                System.out.println("El número secreto es mayor que " + numero);
            } else {
                // Si el número es mayor que el número secreto, decimos que el número secreto es menor
                System.out.println("El número secreto es menor que " + numero);
            }
        }

        // Cerramos el objeto Scanner
        scanner.close();
    }
}
```

**Explicación del código:**

* La clase `Adivinanzas` representa el juego de adivinanzas.
* Los atributos de la clase son:
    * `NUMERO_MAXIMO`: el número máximo que puede generar el juego.
    * `NUMERO_MINIMO`: el número mínimo que puede generar el juego.
    * `numeroSecreto`: el número secreto generado por el juego.
    * `intentos`: el número de intentos que ha hecho el usuario.
* El método `main` es el método principal del juego.
* En el método `main`, primero se crean los objetos `Scanner` y `Random`.
* Luego se genera el número secreto.
* A continuación, se da la bienvenida al usuario y se le explica el juego.
* El bucle principal del juego es un `while` que se ejecuta hasta que el usuario acierta el número secreto.
* Dentro del bucle, se pide al usuario que introduzca un número.
* Se incrementa el número de intentos.
* Se comprueba si el número introducido por el usuario es igual al número secreto.
* Si el número es igual, se felicita al usuario y se sale del bucle.
* Si el número es menor que el número secreto, se dice que el número secreto es mayor.
* Si el número es mayor que el número secreto, se dice que el número secreto es menor.
* Cuando el usuario acierta el número secreto, se cierra el objeto `Scanner`.