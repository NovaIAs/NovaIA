```cool
// Programa que calcula el factorial de un número entero

clase Principal {
    método principal() : Entero {
        variable n : Entero;
        variable factorial : Entero;

        // Solicita al usuario que ingrese un número entero
        escribir("Ingrese un número entero: ");
        n := leerEntero();

        // Inicializa la variable factorial con el valor 1
        factorial := 1;

        // Calcula el factorial de n utilizando un bucle for
        para i en 1 hasta n hacer
            factorial := factorial * i;
        fin para

        // Imprime el factorial de n
        escribir("El factorial de ", n, " es ", factorial);

        // Retorna 0 para indicar que el programa se ejecutó correctamente
        retornar 0;
    }
}
```

Explicación del código:

* La clase `Principal` define el método `principal()`, que es el punto de entrada del programa.
* El método `principal()` declara las variables `n`, `factorial` y `i`.
* La variable `n` es un número entero que el usuario ingresa.
* La variable `factorial` almacena el factorial de `n`.
* La variable `i` es un contador utilizado en el bucle for.
* El método `principal()` utiliza la función `escribir()` para solicitar al usuario que ingrese un número entero.
* La función `leerEntero()` se utiliza para leer el número entero ingresado por el usuario.
* El método `principal()` inicializa la variable `factorial` con el valor 1.
* El método `principal()` utiliza un bucle for para calcular el factorial de `n`. El bucle for incrementa la variable `i` de 1 a `n` y multiplica la variable `factorial` por `i` en cada iteración.
* El método `principal()` utiliza la función `escribir()` para imprimir el factorial de `n`.
* El método `principal()` retorna 0 para indicar que el programa se ejecutó correctamente.