```smalltalk
"**Ejemplo 1: Definición y uso de una clase**

// Definición de la clase Persona
Clase Persona {
    // Atributos
    nombre: String.
    edad: Integer.

    // Constructores
    nueva(nombre: nombre, edad: edad) {
        self nombre: nombre.
        self edad: edad.
    }

    // Métodos
    nombreCompleto {
        "Devuelve el nombre completo de la persona"
        ^ self nombre + " " + self apellido
    }

    esMayorDeEdad {
        "Devuelve true si la persona es mayor de edad"
        ^ self edad > 18
    }
}

// Uso de la clase Persona
persona1 := Persona nueva("Juan", 20).
persona2 := Persona nueva("María", 16).

persona1 nombreCompleto. // "Juan Pérez"
persona2 nombreCompleto. // "María González"

persona1 esMayorDeEdad. // true
persona2 esMayorDeEdad. // false


**Ejemplo 2: Herencia y polimorfismo**

// Definición de la clase Estudiante, que hereda de Persona
Clase Estudiante heredaDe: Persona {
    // Atributos
    curso: String.

    // Constructores
    nueva(nombre: nombre, edad: edad, curso: curso) {
        super nueva(nombre: nombre, edad: edad).
        self curso: curso.
    }

    // Métodos
    curso {
        "Devuelve el curso del estudiante"
        ^ self curso
    }
}

// Definición de la clase Profesor, que hereda de Persona
Clase Profesor heredaDe: Persona {
    // Atributos
    materia: String.

    // Constructores
    nueva(nombre: nombre, edad: edad, materia: materia) {
        super nueva(nombre: nombre, edad: edad).
        self materia: materia.
    }

    // Métodos
    materia {
        "Devuelve la materia del profesor"
        ^ self materia
    }
}

// Uso de las clases Estudiante y Profesor
estudiante1 := Estudiante nueva("Juan", 20, "Ingeniería").
profesor1 := Profesor nueva("María", 35, "Matemáticas").

estudiante1 nombreCompleto. // "Juan Pérez"
profesor1 nombreCompleto. // "María González"

estudiante1 curso. // "Ingeniería"
profesor1 materia. // "Matemáticas"


**Ejemplo 3: Manejo de colecciones**

// Definición de la clase Lista
Clase Lista {
    // Atributos
    elementos: Array.

    // Constructores
    nueva {
        self elementos: Array nueva.
    }

    // Métodos
    agregar(elemento) {
        "Agrega un elemento a la lista"
        self elementos agregar: elemento.
    }

    eliminar(elemento) {
        "Elimina un elemento de la lista"
        self elementos eliminar: elemento.
    }

    longitud {
        "Devuelve la longitud de la lista"
        ^ self elementos longitud.
    }

    elementoEnÍndice(índice) {
        "Devuelve el elemento en el índice especificado"
        ^ self elementos elementoEnÍndice: índice.
    }
}

// Uso de la clase Lista
lista1 := Lista nueva.
lista1 agregar("Juan").
lista1 agregar("María").
lista1 agregar("Pedro").

lista1 longitud. // 3
lista1 elementoEnÍndice(2). // "María"


**Ejemplo 4: Bloques y cierres**

// Definición de un bloque
bloque := [|x| x * 2].

// Uso del bloque
bloque valor: 5. // 10

// Definición de un cierre
cierre := [|x| x * 2].

// Uso del cierre
cierre valor: 5. // 10

// Uso del cierre como argumento de un método
lista1 seleccionar: [|elemento| elemento > "M"]. // ["María", "Pedro"]


**Ejemplo 5: Metaprogramación**

// Definición de una clase meta
ClaseMeta {
    // Métodos
    nuevaClase(nombre: nombre, superclase: superclase, métodos: métodos) {
        "Crea una nueva clase"
        ^ Clase nueva(nombre: nombre, superclase: superclase, métodos: métodos).
    }
}

// Uso de la clase meta para crear una nueva clase
ClasePersona := ClaseMeta nuevaClase(nombre: "Persona", superclase: nil, métodos: {
    nombre: String.
    edad: Integer.
    nombreCompleto {
        "Devuelve el nombre completo de la persona"
        ^ self nombre + " " + self apellido
    }
    esMayorDeEdad {
        "Devuelve true si la persona es mayor de edad"
        ^ self edad > 18
    }
}).

// Creación de una instancia de la nueva clase
persona1 := ClasePersona nueva("Juan", 20).

persona1 nombreCompleto. // "Juan Pérez"
persona1 esMayorDeEdad. // true"
```

**Explicación del código:**

* **Ejemplo 1:** Define una clase `Persona` con atributos, constructores y métodos. Se crea una instancia de la clase y se utilizan sus métodos para obtener el nombre completo y la edad de la persona.
* **Ejemplo 2:** Define dos clases, `Estudiante` y `Profesor`, que heredan de la clase `Persona`. Se crean instancias de las clases y se utilizan sus métodos para obtener el nombre completo, el curso y la materia de la persona.
* **Ejemplo 3:** Define una clase `Lista` que representa una lista de elementos. Se crean instancias de la clase y se utilizan sus métodos para agregar, eliminar, obtener la longitud y obtener el elemento en un índice especificado.
* **Ejemplo 4:** Define un bloque y un cierre. Se utiliza el bloque y el cierre para multiplicar un número por 2. También se utiliza el cierre como argumento de un método para seleccionar elementos de una lista.
* **Ejemplo 5:** Define una clase meta que se utiliza para crear una nueva clase. Se utiliza la clase meta para crear una nueva clase `Persona` con atributos, constructores y métodos. Se crea una instancia de la nueva clase y se utilizan sus métodos para obtener el nombre completo y la edad de la persona.