```java
// Programa que calcula el factorial de un número natural.

// Definimos una clase llamada Factorial.
public class Factorial {

    // Definimos un método estático llamado calcularFactorial que recibe un número natural como parámetro y devuelve su factorial.
    public static int calcularFactorial(int n) {
        // Si el número es menor que 0, lanzamos una excepción.
        if (n < 0) {
            throw new IllegalArgumentException("El número debe ser mayor o igual que 0.");
        }

        // Si el número es 0 o 1, devolvemos 1.
        if (n == 0 || n == 1) {
            return 1;
        }

        // Calculamos el factorial del número llamando recursivamente al método calcularFactorial.
        return n * calcularFactorial(n - 1);
    }

    // Definimos el método principal del programa.
    public static void main(String[] args) {
        // Pedimos al usuario que introduzca un número natural.
        System.out.println("Introduzca un número natural:");
        int n = Integer.parseInt(System.console().readLine());

        // Calculamos el factorial del número utilizando el método calcularFactorial.
        int factorial = calcularFactorial(n);

        // Mostramos el resultado por pantalla.
        System.out.println("El factorial de " + n + " es " + factorial);
    }
}
```

**Explicación del código:**

* La clase `Factorial` contiene un método estático llamado `calcularFactorial` que recibe un número natural como parámetro y devuelve su factorial.
* El método `calcularFactorial` utiliza la recursividad para calcular el factorial del número.
* El método principal del programa pide al usuario que introduzca un número natural y luego calcula su factorial llamando al método `calcularFactorial`.
* El resultado se muestra por pantalla.

**¿Por qué es un código complejo?**

* El código utiliza la recursividad, que es un concepto avanzado de programación.
* El código también utiliza el manejo de excepciones, que es una herramienta importante para manejar errores en tiempo de ejecución.
* El código está bien estructurado y comentado, lo que lo hace fácil de leer y mantener.