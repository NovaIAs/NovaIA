```cool
# Clase que define al programa principal
class Main {
    # Método principal que se ejecuta al iniciar el programa
    main() {
        # Declaración de variables
        var num1: Int;
        var num2: Int;
        var resultado: Int;

        # Lectura de los números del usuario
        println("Introduce el primer número:");
        num1 := readInt();
        println("Introduce el segundo número:");
        num2 := readInt();

        # Cálculo de la suma de los números
        resultado := num1 + num2;

        # Impresión del resultado en la consola
        println("La suma de los números es:");
        printInt(resultado);
    }
}
```

Explicación del código:

1. Se define la clase `Main` que será el punto de entrada del programa.
2. Dentro de la clase `Main` se declara el método `main` que es el método principal que se ejecuta al iniciar el programa.
3. Se declaran las variables `num1`, `num2` y `resultado` que serán utilizadas para almacenar los números introducidos por el usuario y el resultado de la suma.
4. Se utilizan las funciones `println` y `readInt` para leer los números del usuario y mostrar el resultado en la consola.
5. Se calcula la suma de los números utilizando el operador `+`.
6. Se utiliza la función `printInt` para imprimir el resultado en la consola.