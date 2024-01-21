```cool
clase Animal {
    propiedad nombre: String;
    método constructor(nombre: String) {
        this.nombre = nombre;
    }
    método hacerSonido() {
        println("El animal hace un sonido.");
    }
}

clase Perro : Animal {
    propiedad raza: String;
    método constructor(nombre: String, raza: String) : super(nombre) {
        this.raza = raza;
    }
    método hacerSonido() {
        println("El perro ladra.");
    }
}

clase Gato : Animal {
    propiedad color: String;
    método constructor(nombre: String, color: String) : super(nombre) {
        this.color = color;
    }
    método hacerSonido() {
        println("El gato maúlla.");
    }
}

clase Pajaro : Animal {
    propiedad especie: String;
    método constructor(nombre: String, especie: String) : super(nombre) {
        this.especie = especie;
    }
    método hacerSonido() {
        println("El pájaro canta.");
    }
}

clase Main {
    método main() {
        variable perro = new Perro("Firulais", "Pastor Alemán");
        variable gato = new Gato("Michi", "Siamés");
        variable pajaro = new Pajaro("Chiquitín", "Canario");

        perro.hacerSonido();
        gato.hacerSonido();
        pajaro.hacerSonido();
    }
}
```

Explicación:

El código define una clase base `Animal` con una propiedad `nombre` y un método `hacerSonido()`. Dos subclases, `Perro` y `Gato`, heredan de `Animal` y definen sus propias propiedades y métodos. Una clase `Main` contiene el método `main()` que crea instancias de las clases `Perro`, `Gato` y `Pajaro` y llama al método `hacerSonido()` para cada una.

Como COOL es un lenguaje orientado a objetos, se pueden crear clases y objetos para representar entidades del mundo real. En este caso, se crean clases para representar animales, perros, gatos y pájaros. Cada clase tiene sus propias propiedades y métodos, que se pueden utilizar para representar las características y comportamientos de los objetos.

El método `main()` es el punto de entrada del programa. En este método, se crean instancias de las clases `Perro`, `Gato` y `Pajaro` y se llama al método `hacerSonido()` para cada una. Esto hace que el programa imprima en la consola los sonidos que hacen los animales.