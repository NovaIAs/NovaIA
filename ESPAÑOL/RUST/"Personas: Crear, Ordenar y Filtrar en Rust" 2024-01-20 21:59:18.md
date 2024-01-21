```rust
// Definimos una estructura `Persona` que contiene los campos `nombre` y `edad`.
struct Persona {
    nombre: String,
    edad: u8,
}

// Implementamos una función `mostrar_persona` que imprime los campos de una persona.
fn mostrar_persona(persona: &Persona) {
    println!("Nombre: {}", persona.nombre);
    println!("Edad: {}", persona.edad);
}

// Creamos una función `crear_persona` que crea una nueva persona a partir de un nombre y una edad.
fn crear_persona(nombre: String, edad: u8) -> Persona {
    Persona { nombre, edad }
}

// Creamos una función `ordenar_personas` que ordena una lista de personas por edad.
fn ordenar_personas(personas: &mut [Persona]) {
    personas.sort_by(|a, b| a.edad.cmp(&b.edad));
}

// Creamos una función `filtrar_personas` que filtra una lista de personas por edad.
fn filtrar_personas(personas: &[Persona], edad_minima: u8) -> Vec<Persona> {
    personas.iter().filter(|&persona| persona.edad >= edad_minima).cloned().collect()
}

// Creamos una función `main` que es el punto de entrada del programa.
fn main() {
    // Creamos una lista de personas.
    let personas = vec![
        crear_persona("Juan".to_string(), 20),
        crear_persona("María".to_string(), 30),
        crear_persona("Pedro".to_string(), 40),
        crear_persona("Ana".to_string(), 10),
    ];

    // Mostramos las personas en orden aleatorio.
    println!("Personas en orden aleatorio:");
    for persona in &personas {
        mostrar_persona(persona);
    }

    // Ordenamos las personas por edad.
    ordenar_personas(&mut personas);

    // Mostramos las personas ordenadas por edad.
    println!("Personas ordenadas por edad:");
    for persona in &personas {
        mostrar_persona(persona);
    }

    // Filtramos las personas por edad.
    let personas_mayores_de_20 = filtrar_personas(&personas, 20);

    // Mostramos las personas mayores de 20 años.
    println!("Personas mayores de 20 años:");
    for persona in &personas_mayores_de_20 {
        mostrar_persona(persona);
    }
}
```

Este código Rust crea una lista de personas con sus nombres y edades, luego ordena la lista por edad y filtra las personas mayores de 20 años. Finalmente, muestra las personas en orden aleatorio, ordenadas por edad y mayores de 20 años.

A continuación, se explica el código en detalle:

* La estructura `Persona` define un nuevo tipo de dato que contiene los campos `nombre` y `edad`.
* La función `mostrar_persona` imprime los campos de una persona.
* La función `crear_persona` crea una nueva persona a partir de un nombre y una edad.
* La función `ordenar_personas` ordena una lista de personas por edad.
* La función `filtrar_personas` filtra una lista de personas por edad.
* La función `main` es el punto de entrada del programa y crea una lista de personas, ordena la lista, filtra las personas mayores de 20 años y muestra las personas en orden aleatorio, ordenadas por edad y mayores de 20 años.

Este código es un ejemplo de cómo utilizar estructuras, funciones y generadores en Rust para crear un programa que realice una tarea compleja.