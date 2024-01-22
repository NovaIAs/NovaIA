```java
// Clase principal del programa
public class Fibonacci {

    // Método principal del programa
    public static void main(String[] args) {
        // Crear una lista con los primeros números de la serie de Fibonacci
        List<Integer> fibonacci = new ArrayList<>();
        fibonacci.add(0);
        fibonacci.add(1);

        // Calcular los siguientes números de la serie
        while (fibonacci.get(fibonacci.size() - 1) < 1000) {
            int nextNumber = fibonacci.get(fibonacci.size() - 1) + fibonacci.get(fibonacci.size() - 2);
            fibonacci.add(nextNumber);
        }

        // Imprimir los números de la serie
        for (int number : fibonacci) {
            System.out.println(number);
        }
    }
}
```

Explicación del código:

* La clase `Fibonacci` es la clase principal del programa.
* El método `main` es el método principal del programa y se ejecuta cuando se inicia el programa.
* Se crea una lista llamada `fibonacci` para almacenar los números de la serie.
* Se añaden los dos primeros números de la serie a la lista.
* Se utiliza un bucle `while` para calcular los siguientes números de la serie. El bucle se ejecuta mientras el último número de la lista sea menor que 1000.
* Dentro del bucle, se calcula el siguiente número de la serie sumando los dos últimos números de la lista.
* Se añade el siguiente número de la serie a la lista.
* Se utiliza un bucle `for` para imprimir los números de la serie.