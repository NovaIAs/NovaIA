```cool
-- Definición de clases y métodos

class Persona {
    nombre: String;
    edad: Integer;

    constructor(nombre: String, edad: Integer) {
        self.nombre  := nombre;
        self.edad  :=  edad;
    }

    imprimir() {
        IO.out_string("Nombre: ");
        IO.out_string(self.nombre);
        IO.out_string(", Edad: ");
        IO.out_int(self.edad);
        IO.out_string("\n");
    }
}

class Estudiante extends Persona {
    promedio: Real;

    constructor(nombre: String, edad: Integer, promedio: Real) {
        super(nombre, edad);
        self.promedio := promedio;
    }

    imprimir() {
        super.imprimir();
        IO.out_string("Promedio: ");
        IO.out_real(self.promedio);
        IO.out_string("\n");
    }
}

class Profesor extends Persona {
    materia: String;

    constructor(nombre: String, edad: Integer, materia: String) {
        super(nombre, edad);
        self.materia := materia;
    }

    imprimir() {
        super.imprimir();
        IO.out_string("Materia: ");
        IO.out_string(self.materia);
        IO.out_string("\n");
    }
}

-- Creación de objetos

persona1 := new Persona("Juan", 20);
estudiante1 := new Estudiante("María", 22, 3.8);
profesor1 := new Profesor("Pedro", 30, "Matemáticas");

-- Impresión de objetos

persona1.imprimir();
estudiante1.imprimir();
profesor1.imprimir();
```

**Explicación del código:**

Este código implementa un ejemplo sencillo de herencia y polimorfismo en COOL.

* **Definimos tres clases:** `Persona`, `Estudiante` y `Profesor`.
* La clase `Persona` tiene dos atributos: `nombre` y `edad`. También tiene un constructor que inicializa estos atributos.
* La clase `Estudiante` hereda de la clase `Persona`. Tiene un atributo adicional: `promedio`. También tiene un constructor que inicializa todos los atributos.
* La clase `Profesor` también hereda de la clase `Persona`. Tiene un atributo adicional: `materia`. También tiene un constructor que inicializa todos los atributos.
* Creamos tres objetos: una persona, un estudiante y un profesor.
* Imprimimos los objetos usando el método `imprimir()` de cada clase.

El código muestra cómo podemos crear clases y objetos en COOL, y cómo podemos usar la herencia y el polimorfismo para crear jerarquías de clases.