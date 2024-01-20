**Código en JAVA**

```java
// Importación de librerías necesarias
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

// Clase principal
public class CodigoComplejo {

    // Método principal
    public static void main(String[] args) {
        // Creación de un objeto Scanner para leer datos del teclado
        Scanner entrada = new Scanner(System.in);

        // Creación de una lista para almacenar números enteros
        List<Integer> numeros = new ArrayList<>();

        // Solicitar al usuario que ingrese números enteros, hasta que ingrese un número negativo
        System.out.println("Ingrese números enteros (termine con un número negativo):");
        int numero = 0;
        while (numero >= 0) {
            numero = entrada.nextInt();
            if (numero >= 0) {
                numeros.add(numero);
            }
        }

        // Cálculo del promedio de los números ingresados
        int suma = 0;
        for (int num : numeros) {
            suma += num;
        }
        double promedio = (double) suma / numeros.size();

        // Creación de una lista para almacenar números primos
        List<Integer> primos = new ArrayList<>();

        // Búsqueda de números primos en la lista de números ingresados
        for (int num : numeros) {
            boolean esPrimo = true;
            if (num > 1) {
                for (int i = 2; i <= num / 2; i++) {
                    if (num % i == 0) {
                        esPrimo = false;
                        break;
                    }
                }
            } else {
                esPrimo = false;
            }
            if (esPrimo) {
                primos.add(num);
            }
        }

        // Impresión del promedio de los números ingresados
        System.out.println("El promedio de los números ingresados es: " + promedio);

        // Impresión de la lista de números primos encontrados
        System.out.println("Los números primos encontrados son:");
        for (int primo : primos) {
            System.out.print(primo + " ");
        }
        System.out.println();
    }
}
```

**Explicación del código:**

1. El código comienza importando las librerías necesarias para el funcionamiento del programa.

2. A continuación, se crea la clase principal `CodigoComplejo`.

3. El método principal `main` es el punto de entrada del programa.

4. Se crea un objeto `Scanner` para leer datos del teclado.

5. Se crea una lista `numeros` para almacenar los números enteros ingresados por el usuario.

6. Se solicita al usuario que ingrese números enteros, hasta que ingrese un número negativo. Los números ingresados se agregan a la lista `numeros`.

7. Se calcula el promedio de los números ingresados utilizando un bucle `for` para sumar todos los números y dividir la suma por el número de elementos en la lista.

8. Se crea una lista `primos` para almacenar los números primos encontrados en la lista de números ingresados.

9. Se realiza una búsqueda de números primos en la lista de números ingresados utilizando otro bucle `for`. Para cada número, se comprueba si es primo utilizando otro bucle `for` que recorre todos los números desde 2 hasta la mitad del número actual. Si el número no es divisible por ninguno de estos números, es primo y se agrega a la lista `primos`.

10. Finalmente, se imprime el promedio de los números ingresados y la lista de números primos encontrados.