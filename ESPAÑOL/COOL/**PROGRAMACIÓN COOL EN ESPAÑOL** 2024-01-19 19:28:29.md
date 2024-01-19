```cool
-- Definición de clases

class Programa {
    main() : String {
        return "Hola, mundo!";
    }
}

class Persona {
    nombre : String;
    edad : Int;

    constructor(nombre : String, edad : Int) {
        this.nombre = nombre;
        this.edad = edad;
    }

    hablar() : String {
        return "Hola, mi nombre es " + nombre + " y tengo " + edad + " años.";
    }
}

class Estudiante : Persona {
    calificaciones : Array[Int];

    constructor(nombre : String, edad : Int, calificaciones : Array[Int]) {
        super(nombre, edad);
        this.calificaciones = calificaciones;
    }

    calcularPromedio() : Float {
        var suma = 0;
        for (calificacion in calificaciones) {
            suma += calificacion;
        }
        return suma / calificaciones.length;
    }
}

-- Creación de objetos

var programa = new Programa();
var persona1 = new Persona("Juan", 20);
var persona2 = new Estudiante("María", 22, [9, 8, 7]);

-- Impresión de resultados

println(programa.main());
println(persona1.hablar());
println(persona2.hablar());
println(persona2.calcularPromedio());
```

**Explicación del código:**

* El código define tres clases: `Programa`, `Persona` y `Estudiante`.
* La clase `Programa` contiene el método `main`, que es el punto de entrada del programa.
* La clase `Persona` representa a una persona y tiene atributos para el nombre y la edad.
* La clase `Estudiante` hereda de la clase `Persona` y añade un atributo para las calificaciones.
* El código crea objetos de las clases `Programa`, `Persona` y `Estudiante` y llama a sus métodos para imprimir resultados.

**El código es complejo porque:**

* Utiliza herencia y polimorfismo.
* Utiliza estructuras de datos como arrays.
* Utiliza bucles y condicionales.
* Realiza cálculos matemáticos.
* Imprime resultados en pantalla.