```cool
clase Persona {
    tipo: String;
    edad: Int;

    constructor(tipo: String, edad: Int) {
        this.tipo = tipo;
        this.edad = edad;
    }

    imprimir() {
        console.log("Soy una persona, mi tipo es " + this.tipo + " y mi edad es " + this.edad);
    }
}

clase Hombre extends Persona {
    constructor(edad: Int) {
        super("hombre", edad);
    }
}

clase Mujer extends Persona {
    constructor(edad: Int) {
        super("mujer", edad);
    }
}

clase Niño extends Persona {
    constructor(edad: Int) {
        super("niño", edad);
    }
}

clase Niña extends Persona {
    constructor(edad: Int) {
        super("niña", edad);
    }
}

var personas = [
    new Hombre(25),
    new Mujer(30),
    new Niño(10),
    new Niña(12)
];

for (var i = 0; i < personas.length; i++) {
    personas[i].imprimir();
}
```

Explicación del código:

* La clase `Persona` es una clase base que define las propiedades y métodos comunes a todas las personas.
* Las clases `Hombre`, `Mujer`, `Niño` y `Niña` son clases derivadas de la clase `Persona` que definen propiedades y métodos específicos para cada tipo de persona.
* El constructor de la clase `Persona` toma dos parámetros: `tipo` y `edad`. El parámetro `tipo` es una cadena de caracteres que indica el tipo de persona (hombre, mujer, niño o niña). El parámetro `edad` es un número entero que indica la edad de la persona.
* El método `imprimir()` de la clase `Persona` imprime en la consola el tipo y la edad de la persona.
* La clase `personas` es un array de objetos de tipo `Persona`.
* El bucle `for` itera sobre el array `personas` e invoca el método `imprimir()` para cada objeto.

Este código crea un array de objetos de tipo `Persona` que representan a cuatro personas: un hombre de 25 años, una mujer de 30 años, un niño de 10 años y una niña de 12 años. El bucle `for` itera sobre el array e invoca el método `imprimir()` para cada objeto, imprimiendo en la consola el tipo y la edad de cada persona.