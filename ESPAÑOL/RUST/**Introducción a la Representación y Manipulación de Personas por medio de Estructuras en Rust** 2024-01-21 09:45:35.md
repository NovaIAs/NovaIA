```
// Definición de un struct para representar una persona.
struct Persona {
    nombre: String,
    edad: u8,
    ocupacion: String,
}

// Función para crear una nueva persona.
fn crear_persona(nombre: String, edad: u8, ocupacion: String) -> Persona {
    Persona {
        nombre,
        edad,
        ocupacion,
    }
}

// Función para imprimir la información de una persona.
fn imprimir_persona(persona: &Persona) {
    println!("Nombre: {}", persona.nombre);
    println!("Edad: {}", persona.edad);
    println!("Ocupación: {}", persona.ocupacion);
}

// Función principal del programa.
fn main() {
    // Creación de una lista de personas.
    let personas: Vec<Persona> = vec![
        crear_persona(String::from("Juan"), 25, String::from("Estudiante")),
        crear_persona(String::from("María"), 30, String::from("Médica")),
        crear_persona(String::from("Pedro"), 40, String::from("Ingeniero")),
    ];

    // Iteración sobre la lista de personas.
    for persona in personas {
        imprimir_persona(&persona);
        println!();
    }
}
```

Explicación:

1. **struct Persona**: definimos un struct llamado `Persona`, que sirve para representar la información de una persona. El struct tiene tres campos: `nombre`, `edad` y `ocupacion`, de tipo `String`, `u8` y `String`, respectivamente.


2. **fn crear_persona**: definimos una función llamada `crear_persona` que recibe tres parámetros: `nombre`, `edad` y `ocupacion`, y devuelve una nueva instancia del struct `Persona`.


3. **fn imprimir_persona**: definimos una función llamada `imprimir_persona`, que recibe una referencia a una variable tipo `Persona` y la imprime.


4. **fn main**: definimos la función `main`, que será la primera que se ejecute al llamar al programa.


5. **Creación de la lista de personas**: creamos una lista de personas usando `vec!`, que es una macro para crear un vector, que es un tipo de colección en Rust.


6. **Iteración sobre la lista de personas**: iteramos sobre la lista de personas, llamando a la función `imprimir_persona` para cada elemento.


Este código es complejo ya que combina varias características de Rust, como los structs, las funciones, los tipos de colección y la iteración sobre colecciones. El uso de Rust como lenguaje de programación permite la creación de código elegante y seguro sin tener que preocuparse por la gestión de la memoria o la seguridad de los tipos.