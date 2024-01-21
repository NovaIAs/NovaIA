```cool
-- Definición de clases

clase Persona {
    nombre : String;
    edad : Int;

    constructor(nombre: String, edad: Int) {
        this.nombre = nombre;
        this.edad = edad;
    }

    método presentarse() {
        "Hola, mi nombre es " + nombre + " y tengo " + edad + " años.";
    }
}

clase Estudiante : Persona {
    carrera : String;

    constructor(nombre: String, edad: Int, carrera: String) {
        super(nombre, edad);
        this.carrera = carrera;
    }

    método estudiar() {
        "Estoy estudiando " + carrera + ".";
    }
}

clase Profesor : Persona {
    materia : String;

    constructor(nombre: String, edad: Int, materia: String) {
        super(nombre, edad);
        this.materia = materia;
    }

    método enseñar() {
        "Estoy enseñando " + materia + ".";
    }

}

-- Creación de objetos

persona1 = new Persona("Juan", 20);
estudiante1 = new Estudiante("María", 22, "Ingeniería Informática");
profesor1 = new Profesor("Pedro", 30, "Matemáticas");

-- Llamada a métodos de los objetos

persona1.presentarse();
estudiante1.presentarse();
estudiante1.estudiar();
profesor1.presentarse();
profesor1.enseñar();

-- Funciones auxiliares

función suma(a: Int, b: Int) : Int {
    return a + b;
}

función multiplicación(a: Int, b: Int) : Int {
    return a * b;
}

función factorial(n: Int) : Int {
    if (n == 0) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

-- Uso de las funciones auxiliares

suma(1, 2);
multiplicación(3, 4);
factorial(5);
```

**Explicación del código:**

1. **Definición de clases:** Se definen tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` es la clase base, y las clases `Estudiante` y `Profesor` son clases derivadas.

2. **Constructores:** Cada clase tiene un constructor que inicializa los atributos de los objetos.

3. **Métodos:** Cada clase tiene varios métodos que definen el comportamiento de los objetos. Por ejemplo, la clase `Persona` tiene el método `presentarse()` que muestra el nombre y la edad de la persona.

4. **Creación de objetos:** Se crean tres objetos: `persona1`, `estudiante1` y `profesor1`. Los objetos `estudiante1` y `profesor1` son objetos de las clases derivadas `Estudiante` y `Profesor`, respectivamente.

5. **Llamada a métodos de los objetos:** Se llaman a los métodos de los objetos para mostrar el nombre, la edad, la carrera y la materia de las personas.

6. **Funciones auxiliares:** Se definen tres funciones auxiliares: `suma()`, `multiplicación()` y `factorial()`. Estas funciones no están asociadas a ninguna clase, sino que son funciones globales.

7. **Uso de las funciones auxiliares:** Se llaman a las funciones auxiliares para calcular la suma, la multiplicación y el factorial de algunos números.