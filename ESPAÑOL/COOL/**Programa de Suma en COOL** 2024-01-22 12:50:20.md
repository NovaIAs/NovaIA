```cool
class Programa {

  main() {
    entero x, y;
    x := 5;
    y := 2;
    print(suma(x, y));
  }

  suma(entero x, entero y) : entero {
    return x + y;
  }

}
```

Explicación:

* La clase `Programa` es la clase principal del programa.
* El método `main` es el método principal del programa.
* Las variables `x` e `y` son variables enteras.
* La variable `x` se inicializa con el valor 5.
* La variable `y` se inicializa con el valor 2.
* La instrucción `print(suma(x, y))` imprime el resultado de la suma de `x` e `y`.
* El método `suma` es un método que calcula la suma de dos enteros.
* El método `suma` toma dos parámetros enteros, `x` e `y`.
* El método `suma` devuelve el resultado de la suma de `x` e `y`.