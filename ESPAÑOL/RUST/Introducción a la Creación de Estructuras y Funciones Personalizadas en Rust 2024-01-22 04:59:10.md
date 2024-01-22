```rust
// Creación de una estructura para representar una persona.
#[derive(Debug)]
struct Persona {
    nombre: String,
    edad: u8,
    aficiones: Vec<String>,
}

// Función para crear una nueva persona.
fn crear_persona(nombre: &str, edad: u8, aficiones: Vec<String>) -> Persona {
    Persona {
        nombre: nombre.to_string(),
        edad: edad,
        aficiones: aficiones,
    }
}

// Función para imprimir la información de una persona.
fn imprimir_persona(persona: &Persona) {
    println!("Nombre: {}", persona.nombre);
    println!("Edad: {}", persona.edad);
    println!("Aficiones:");
    for aficion in &persona.aficiones {
        println!("\t- {}", aficion);
    }
}

// Función principal.
fn main() {
    // Creación de una lista de personas.
    let personas = vec![
        crear_persona("Juan", 20, vec!["Futbol", "Música", "Viajes"]),
        crear_persona("María", 25, vec!["Lectura", "Cine", "Yoga"]),
        crear_persona("Pedro", 30, vec!["Programación", "Videojuegos", "Fotografía"]),
    ];

    // Iteración sobre la lista de personas e impresión de su información.
    for persona in &personas {
        imprimir_persona(persona);
        println!();
    }
}
```

Este código crea una estructura `Persona` para representar una persona con los atributos nombre, edad y aficiones. Luego define una función `crear_persona` para crear una nueva persona a partir de estos atributos. También define una función `imprimir_persona` para imprimir la información de una persona. En la función principal, crea una lista de personas usando la función `crear_persona` y luego itera sobre la lista e imprime la información de cada persona usando la función `imprimir_persona`.

Este código es complejo porque combina varias características del lenguaje Rust, como estructuras, funciones, iteración y manejo de listas. También demuestra cómo definir y usar una estructura personalizada en Rust.