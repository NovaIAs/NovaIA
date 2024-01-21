```rust
// Definición de una estructura "Persona" con campos "nombre" y "edad".
struct Persona {
    nombre: String,
    edad: u8,
}

// Función para crear una instancia de la estructura "Persona".
fn crear_persona(nombre: &str, edad: u8) -> Persona {
    Persona {
        nombre: String::from(nombre),
        edad: edad,
    }
}

// Función para imprimir los datos de una instancia de la estructura "Persona".
fn imprimir_persona(persona: &Persona) {
    println!("Nombre: {}, Edad: {}", persona.nombre, persona.edad);
}

// Función principal del programa.
fn main() {
    // Creamos una lista de personas.
    let lista_personas: Vec<Persona> = vec![
        crear_persona("Juan", 25),
        crear_persona("María", 30),
        crear_persona("Pedro", 35),
    ];

    // Iteramos sobre la lista de personas e imprimimos los datos de cada una.
    for persona in lista_personas {
        imprimir_persona(&persona);
    }
}
```

Este código define una estructura `Persona` con dos campos, `nombre` y `edad`, y luego crea una lista de personas usando la función `crear_persona`. Finalmente, itera sobre la lista de personas e imprime los datos de cada una.

El código está organizado en tres funciones: `crear_persona`, `imprimir_persona` y `main`. La función `crear_persona` toma dos argumentos, un nombre y una edad, y devuelve una instancia de la estructura `Persona`. La función `imprimir_persona` toma una instancia de la estructura `Persona` y la imprime en la pantalla. La función `main` es la función principal del programa, y llama a las otras funciones para crear una lista de personas e imprimirlas en la pantalla.

El código utiliza varios conceptos de Rust, como estructuras, listas, iteradores y funciones. También utiliza el sistema de tipos de Rust para garantizar que los argumentos pasados a las funciones sean del tipo correcto.