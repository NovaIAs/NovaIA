```cool
-- Programa que calcula el área de un triángulo.

-- Definición de la clase Triángulo.
class Triángulo {

    -- Atributos: base y altura.
    base: Real;
    altura: Real;

    -- Constructor de la clase.
    constructor Triángulo(base: Real, altura: Real) {
        this.base := base;
        this.altura := altura;
    }

    -- Método para calcular el área del triángulo.
    method área(): Real {
        return (this.base * this.altura) / 2;
    }
}

-- Definición de la clase PruebaTriángulo.
class PruebaTriángulo {

    -- Método principal.
    method main() {

        -- Creación de un objeto de la clase Triángulo.
        var triángulo: Triángulo := new Triángulo(5, 10);

        -- Impresión del área del triángulo.
        IO.out_string("El área del triángulo es: ");
        IO.out_real(triángulo.área());
        IO.out_char('\n');
    }
}
```

Explicación del código:

* La clase `Triángulo` define los atributos `base` y `altura`, y el constructor de la clase inicializa estos atributos con los valores pasados como argumentos.


* El método `área()` de la clase `Triángulo` calcula el área del triángulo utilizando la fórmula `(base * altura) / 2`.


* La clase `PruebaTriángulo` define el método `main()`, que es el punto de entrada del programa.


* El método `main()` crea un objeto de la clase `Triángulo` con los valores `base = 5` y `altura = 10`.


* El método `main()` imprime en la consola el área del triángulo utilizando el método `área()` del objeto `triángulo`.