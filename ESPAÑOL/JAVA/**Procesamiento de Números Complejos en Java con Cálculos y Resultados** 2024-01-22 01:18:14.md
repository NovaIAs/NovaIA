```java
// Programa complejo en Java (versión completa con explicaciones)

// Importar bibliotecas necesarias
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class CodigoComplejo {

    public static void main(String[] args) {
        // Crear un objeto Scanner para leer entrada del usuario
        Scanner entrada = new Scanner(System.in);

        // Crear una lista de números enteros
        List<Integer> numeros = new ArrayList<>();

        // Solicitar al usuario que ingrese números enteros, y agregarlos a la lista
        System.out.println("Ingrese una serie de números enteros, separados por espacios:");
        String input = entrada.nextLine();
        String[] numerosStr = input.split(" ");
        for (String numero : numerosStr) {
            int numeroInt = Integer.parseInt(numero);
            numeros.add(numeroInt);
        }

        // Calcular la suma de todos los números en la lista
        int suma = 0;
        for (int numero : numeros) {
            suma += numero;
        }

        // Calcular el promedio de todos los números en la lista
        double promedio = (double) suma / numeros.size();

        // Calcular el número mayor de la lista
        int mayor = Integer.MIN_VALUE;
        for (int numero : numeros) {
            if (numero > mayor) {
                mayor = numero;
            }
        }

        // Calcular el número menor de la lista
        int menor = Integer.MAX_VALUE;
        for (int numero : numeros) {
            if (numero < menor) {
                menor = numero;
            }
        }

        // Mostrar los resultados al usuario
        System.out.println("La suma de todos los números es: " + suma);
        System.out.println("El promedio de todos los números es: " + promedio);
        System.out.println("El número mayor de la lista es: " + mayor);
        System.out.println("El número menor de la lista es: " + menor);
    }
}
```

**Explicación del código:**

1. **Importar bibliotecas necesarias:**

   ```java
   import java.util.Scanner;
   import java.util.ArrayList;
   import java.util.List;
   ```

   Estas líneas importan las bibliotecas necesarias para trabajar con entrada del usuario, listas y operaciones matemáticas básicas.

2. **Crear una lista de números enteros:**

   ```java
   List<Integer> numeros = new ArrayList<>();
   ```

   Esta línea crea una lista de números enteros vacía, utilizando la clase `ArrayList`.

3. **Solicitar al usuario que ingrese números enteros y agregarlos a la lista:**

   ```java
   System.out.println("Ingrese una serie de números enteros, separados por espacios:");
   String input = entrada.nextLine();
   String[] numerosStr = input.split(" ");
   for (String numero : numerosStr) {
       int numeroInt = Integer.parseInt(numero);
       numeros.add(numeroInt);
   }
   ```

   Estas líneas solicitan al usuario que ingrese una serie de números enteros, separados por espacios, y los agrega a la lista `numeros`.

4. **Calcular la suma de todos los números en la lista:**

   ```java
   int suma = 0;
   for (int numero : numeros) {
       suma += numero;
   }
   ```

   Estas líneas calculan la suma de todos los números en la lista `numeros` utilizando un bucle `for` y acumulando los valores en la variable `suma`.

5. **Calcular el promedio de todos los números en la lista:**

   ```java
   double promedio = (double) suma / numeros.size();
   ```

   Esta línea calcula el promedio de todos los números en la lista `numeros` dividiendo la suma total por el número de elementos en la lista.

6. **Calcular el número mayor de la lista:**

   ```java
   int mayor = Integer.MIN_VALUE;
   for (int numero : numeros) {
       if (numero > mayor) {
           mayor = numero;
       }
   }
   ```

   Estas líneas calculan el número mayor de la lista `numeros` utilizando un bucle `for` y manteniendo un seguimiento del número mayor actual en la variable `mayor`.

7. **Calcular el número menor de la lista:**

   ```java
   int menor = Integer.MAX_VALUE;
   for (int numero : numeros) {
       if (numero < menor) {
           menor = numero;
       }
   }
   ```

   Estas líneas calculan el número menor de la lista `numeros` utilizando un bucle `for` y manteniendo un seguimiento del número menor actual en la variable `menor`.

8. **Mostrar los resultados al usuario:**

   ```java
   System.out.println("La suma de todos los números es: " + suma);
   System.out.println("El promedio de todos los números es: " + promedio);
   System.out.println("El número mayor de la lista es: " + mayor);
   System.out.println("El número menor de la lista es: " + menor);
   ```

   Estas líneas muestran los resultados calculados al usuario, incluyendo la suma total, el promedio, el número mayor y el número menor.