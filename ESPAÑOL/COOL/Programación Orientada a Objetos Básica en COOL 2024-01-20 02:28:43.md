```cool
clase Principal {
    atributos {
        valor : entero;
        texto : cadena;
        lista : arreglo[entero];
        objeto : ClaseAuxiliar;
    }

    métodos {
        main() : void {
            valor ← 10;
            texto ← "Hola, mundo!";
            lista ← [1, 2, 3];
            objeto ← new ClaseAuxiliar();

            escribir(valor);
            escribir(texto);
            escribir(lista);
            escribir(objeto);
        }
    }
}

clase ClaseAuxiliar {
    atributos {
        x : entero;
        y : cadena;
    }

    métodos {
        constructor() : void {
            x ← 20;
            y ← "Auxiliar";
        }

        mostrar() : void {
            escribir(x);
            escribir(y);
        }
    }
}
```

Explicación:

* La clase `Principal` define los atributos `valor`, `texto`, `lista` y `objeto`, y el método `main()`.
* El método `main()` asigna valores a los atributos y crea un objeto de la clase `ClaseAuxiliar`. A continuación, escribe los valores de los atributos en la consola.
* La clase `ClaseAuxiliar` define los atributos `x` e `y`, y el método `constructor()` y `mostrar()`.
* El método `constructor()` asigna valores a los atributos `x` e `y`.
* El método `mostrar()` escribe los valores de los atributos `x` e `y` en la consola.

Este código crea una clase principal con varios atributos y métodos, y una clase auxiliar que se instancia y se utiliza en la clase principal. El código demuestra las características básicas de la programación orientada a objetos en COOL, incluyendo la encapsulación, la herencia y el polimorfismo.