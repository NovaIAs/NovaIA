```java
// Importación de librerías
import java.util.Scanner;
import java.lang.Math;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

// Clase principal
public class Complejo {

    // Método principal
    public static void main(String[] args) {
        // Creamos un objeto Scanner para leer datos de la consola
        Scanner scanner = new Scanner(System.in);

        // Pedimos al usuario que introduzca un número
        System.out.println("Introduzca un número:");
        int numero = scanner.nextInt();

        // Comprobamos si el número es par o impar
        if (numero % 2 == 0) {
            // Si el número es par, imprimimos un mensaje
            System.out.println("El número es par.");
        } else {
            // Si el número es impar, imprimimos un mensaje
            System.out.println("El número es impar.");
        }

        // Pedimos al usuario que introduzca una cadena de texto
        System.out.println("Introduzca una cadena de texto:");
        String texto = scanner.nextLine();

        // Convertimos la cadena de texto a minúsculas
        texto = texto.toLowerCase();

        // Contamos el número de vocales en la cadena de texto
        int numeroVocales = 0;
        for (char caracter : texto.toCharArray()) {
            if (caracter == 'a' || caracter == 'e' || caracter == 'i' || caracter == 'o' || caracter == 'u') {
                numeroVocales++;
            }
        }

        // Imprimimos el número de vocales en la cadena de texto
        System.out.println("El número de vocales en la cadena de texto es: " + numeroVocales);

        // Creamos una lista de números
        List<Integer> numeros = Arrays.asList(1, 2, 3, 4, 5);

        // Filtramos los números pares de la lista
        List<Integer> numerosPares = numeros.stream().filter(numero -> numero % 2 == 0).collect(Collectors.toList());

        // Imprimimos los números pares de la lista
        System.out.println("Los números pares de la lista son: " + numerosPares);

        // Creamos un objeto de la clase Math
        Math math = new Math();

        // Calculamos la raíz cuadrada de un número
        double raizCuadrada = math.sqrt(9);

        // Imprimimos la raíz cuadrada del número
        System.out.println("La raíz cuadrada de 9 es: " + raizCuadrada);
    }
}
```

Explicación del código:

* El código comienza importando las librerías necesarias: `Scanner`, `Math`, `Arrays`, `List` y `Collectors`.
* La clase principal se llama `Complejo`.
* El método principal `main` es el punto de entrada del programa.
* Se crea un objeto `Scanner` para leer datos de la consola.
* Se pide al usuario que introduzca un número y se guarda en la variable `numero`.
* Se comprueba si el número es par o impar y se imprime un mensaje correspondiente.
* Se pide al usuario que introduzca una cadena de texto y se guarda en la variable `texto`.
* Se convierte la cadena de texto a minúsculas.
* Se cuenta el número de vocales en la cadena de texto y se guarda en la variable `numeroVocales`.
* Se imprime el número de vocales en la cadena de texto.
* Se crea una lista de números y se guarda en la variable `numeros`.
* Se filtran los números pares de la lista y se guarda en la variable `numerosPares`.
* Se imprime la lista de números pares.
* Se crea un objeto de la clase `Math` y se guarda en la variable `math`.
* Se calcula la raíz cuadrada de un número y se guarda en la variable `raizCuadrada`.
* Se imprime la raíz cuadrada del número.