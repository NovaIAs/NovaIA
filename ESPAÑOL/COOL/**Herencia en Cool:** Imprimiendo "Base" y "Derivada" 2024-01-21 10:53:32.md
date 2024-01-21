```cool
clase Base {
    método imprimir() {
        impresión "Base";
    }
}

clase Derivada : Base {
    método imprimir() {
        impresión "Derivada";
    }
}

clase Principal {
    método principal() {
        variable base: Base <- nuevo Base();
        variable derivada: Derivada <- nuevo Derivada();

        base.imprimir();
        derivada.imprimir();
    }
}
```

Explicación:

* La clase `Base` define un método `imprimir()` que imprime el texto "Base".
* La clase `Derivada` hereda de la clase `Base` y también define un método `imprimir()` que imprime el texto "Derivada".
* La clase `Principal` define un método `principal()` que es el punto de entrada del programa.
* En el método `principal()`, se crean dos objetos: uno de la clase `Base` y otro de la clase `Derivada`.
* A continuación, se llaman los métodos `imprimir()` de los dos objetos, lo que imprime "Base" y "Derivada" respectivamente.