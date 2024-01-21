```cool
clase Principal {
    atributo contador = 0;

    método principal() {
        mientras (contador < 10) {
            si (contador % 2 == 0) {
                escribe("El número ", contador, " es par.");
            } else {
                escribe("El número ", contador, " es impar.");
            }
            contador += 1;
        }
    }
}
```

Explicación:

* La clase `Principal` define un atributo `contador` que se inicializa con el valor 0.
* El método `principal()` es el punto de entrada del programa.
* El bucle `mientras` se ejecuta mientras el valor de `contador` sea menor que 10.
* Dentro del bucle, hay una instrucción `si` que comprueba si el valor de `contador` es par (usando el operador `%`). Si es par, escribe el mensaje "El número X es par" en la consola. Si no es par, escribe el mensaje "El número X es impar" en la consola.
* Después de ejecutar la instrucción `si`, el valor de `contador` se incrementa en 1.
* El programa se detiene cuando el valor de `contador` es igual a 10.