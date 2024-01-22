```cool
clase Principal {
  entero num;
  entero[] vec;
  cadena cad;

  método Principal() {
    num = 10;
    vec = [1, 2, 3, 4, 5];
    cad = "Hola mundo";

    escribir(num);
    escribir(vec);
    escribir(cad);
  }

  método escribir(entero num) {
    consola.escribir(num);
  }

  método escribir(entero[] vec) {
    consola.escribir("[");
    para (entero i = 0; i < vec.length; i++) {
      escribir(vec[i]);
      si (i != vec.length - 1) {
        consola.escribir(", ");
      }
    }
    consola.escribir("]");
  }

  método escribir(cadena cad) {
    consola.escribir(cad);
  }
}

clase Sumador {
  entero sumar(entero a, entero b) {
    retornar a + b;
  }
}

clase Principal2 {
  Sumador sumador;

  método Principal2() {
    sumador = new Sumador();

    entero resultado = sumador.sumar(2, 3);
    escribir(resultado);
  }

  método escribir(entero resultado) {
    consola.escribir(resultado);
  }
}
```

El código anterior es un ejemplo de un programa en COOL que consta de dos clases: `Principal` y `Sumador`.

La clase `Principal` tiene tres atributos: `num`, `vec` y `cad`. El atributo `num` es un entero, el atributo `vec` es un array de enteros y el atributo `cad` es una cadena de caracteres.

La clase `Principal` tiene dos métodos: `Principal` y `escribir`. El método `Principal` es el constructor de la clase y el método `escribir` es un método que escribe en la consola el valor de un entero, un array de enteros o una cadena de caracteres.

La clase `Sumador` tiene un método llamado `sumar` que recibe dos enteros como parámetros y retorna la suma de los mismos.

La clase `Principal2` tiene un atributo llamado `sumador` que es una instancia de la clase `Sumador`.

El método `Principal2` es el constructor de la clase `Principal2`. En el constructor se crea una instancia de la clase `Sumador` y se almacena en el atributo `sumador`.

Después de crear la instancia de la clase `Sumador`, se llama al método `sumar` de la clase `Sumador` para sumar dos enteros y el resultado se almacena en la variable `resultado`.

Finalmente, se llama al método `escribir` de la clase `Principal2` para escribir el valor de la variable `resultado` en la consola.