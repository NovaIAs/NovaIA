```java
import java.util.Scanner;
import java.util.Arrays;
import java.util.ArrayList;

public class CodigoComplejo {

    private static final Scanner entrada = new Scanner(System.in);

    public static void main(String[] args) {
        // Variables locales
        int opcion, numero, resultado;
        double promedio;
        String nombre, cadena;
        boolean esPrimo;

        // Menú principal
        do {
            System.out.println("\nMenú principal:");
            System.out.println("1. Obtener el factorial de un número");
            System.out.println("2. Calcular el promedio de una serie de números");
            System.out.println("3. Comprobar si un número es primo");
            System.out.println("4. Buscar una palabra en una cadena");
            System.out.println("5. Salir");
            System.out.print("Elige una opción: ");
            opcion = entrada.nextInt();

            // Procesar la opción elegida
            switch (opcion) {
                case 1:
                    // Obtener el factorial de un número
                    System.out.print("Introduce un número: ");
                    numero = entrada.nextInt();
                    resultado = factorial(numero);
                    System.out.println("El factorial de " + numero + " es " + resultado);
                    break;
                case 2:
                    // Calcular el promedio de una serie de números
                    System.out.print("Introduce una serie de números separados por espacios: ");
                    cadena = entrada.nextLine();
                    ArrayList<Integer> numeros = new ArrayList<>();
                    for (String numeroStr : cadena.split(" ")) {
                        numeros.add(Integer.parseInt(numeroStr));
                    }
                    promedio = promedio(numeros);
                    System.out.println("El promedio de los números introducidos es " + promedio);
                    break;
                case 3:
                    // Comprobar si un número es primo
                    System.out.print("Introduce un número: ");
                    numero = entrada.nextInt();
                    esPrimo = esPrimo(numero);
                    System.out.println(numero + (esPrimo ? " es un número primo" : " no es un número primo"));
                    break;
                case 4:
                    // Buscar una palabra en una cadena
                    System.out.print("Introduce una cadena: ");
                    cadena = entrada.nextLine();
                    System.out.print("Introduce la palabra a buscar: ");
                    nombre = entrada.nextLine();
                    int posicion = cadena.indexOf(nombre);
                    if (posicion == -1) {
                        System.out.println("La palabra \"" + nombre + "\" no se encuentra en la cadena \"" + cadena + "\"");
                    } else {
                        System.out.println("La palabra \"" + nombre + "\" se encuentra en la cadena \"" + cadena + "\" en la posición " + posicion);
                    }
                    break;
                case 5:
                    // Salir del programa
                    System.out.println("Saliendo del programa...");
                    break;
                default:
                    // Opción no válida
                    System.out.println("Opción no válida. Introduce un número del 1 al 5");
            }
        } while (opcion != 5);
    }

    // Método para calcular el factorial de un número
    private static int factorial(int numero) {
        if (numero == 0) {
            return 1;
        } else {
            return numero * factorial(numero - 1);
        }
    }

    // Método para calcular el promedio de una serie de números
    private static double promedio(ArrayList<Integer> numeros) {
        int suma = 0;
        for (int numero : numeros) {
            suma += numero;
        }
        return (double) suma / numeros.size();
    }

    // Método para comprobar si un número es primo
    private static boolean esPrimo(int numero) {
        if (numero <= 1) {
            return false;
        }
        for (int i = 2; i <= Math.sqrt(numero); i++) {
            if (numero % i == 0) {
                return false;
            }
        }
        return true;
    }
}
```

Explicación del código:

* El código principal se encuentra en el método `main`, que es el punto de entrada del programa.
* Se utiliza un menú para que el usuario elija la operación que desea realizar.
* Las operaciones disponibles son:
    * Calcular el factorial de un número.
    * Calcular el promedio de una serie de números.
    * Comprobar si un número es primo.
    * Buscar una palabra en una cadena.
    * Salir del programa.
* Para cada operación, se utilizan métodos auxiliares para realizar los cálculos necesarios.
* El método `factorial()` calcula el factorial de un número utilizando recursividad.
* El método `promedio()` calcula el promedio de una serie de números utilizando un bucle `for`.
* El método `esPrimo()` comprueba si un número es primo utilizando un bucle `for`.
* El método `indexOf()` se utiliza para buscar una palabra en una cadena.
* El programa utiliza la clase `Scanner` para leer los datos introducidos por el usuario.
* El programa utiliza la clase `ArrayList` para almacenar una serie de números.