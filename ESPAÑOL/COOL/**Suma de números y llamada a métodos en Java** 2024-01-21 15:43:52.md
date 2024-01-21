```cool
clase MetodoPrincipal {

  entero[] array = {1, 2, 3, 4, 5};

  vacio metodoPrincipal() {
    entero resultado = 0;

    para (entero i = 0; i < array.largo; i++) {
      resultado = resultado + array[i];
    }

    escribe("El resultado de la suma es: ", resultado);
  }

}

clase MetodoSecundario {

  entero sumar(entero a, entero b) {
    return a + b;
  }

}

clase MetodoOtro {

  vacio imprimir(String mensaje) {
    escribe("El mensaje es: ", mensaje);
  }

}

clase Principal {

  vacio metodoPrincipal() {
    MetodoSecundario metodoSecundario = new MetodoSecundario();
    MetodoOtro metodoOtro = new MetodoOtro();

    entero resultado = metodoSecundario.sumar(10, 20);
    metodoOtro.imprimir("El resultado es: " + resultado);
  }

}
```

Explicación:

* Clase `MetodoPrincipal`: Esta clase contiene el método `metodoPrincipal`.
* Clase `MetodoSecundario`: Esta clase contiene el método `sumar`.
* Clase `MetodoOtro`: Esta clase contiene el método `imprimir`.
* Clase `Principal`: Esta clase contiene el método `metodoPrincipal`.
* Método `metodoPrincipal`: Este método es el punto de entrada del programa. Crea objetos de las clases `MetodoSecundario` y `MetodoOtro`. Luego llama al método `sumar` de la clase `MetodoSecundario` y al método `imprimir` de la clase `MetodoOtro`.
* Método `sumar`: Este método suma dos enteros y devuelve el resultado.
* Método `imprimir`: Este método imprime un mensaje en la consola.
* Array `array`: Este array contiene los números del 1 al 5.
* Variable `resultado`: Esta variable almacena el resultado de la suma de los números del array.

Salida:

```
El resultado de la suma es: 15
El mensaje es: El resultado es: 30
```