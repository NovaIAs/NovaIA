```rust
// Definición de una estructura "Persona" con campos "nombre" y "edad".
struct Persona {
    nombre: String,
    edad: u8,
}

// Función para crear una nueva instancia de la estructura "Persona".
fn crear_persona(nombre: String, edad: u8) -> Persona {
    Persona {
        nombre,
        edad,
    }
}

// Función para imprimir los datos de una instancia de la estructura "Persona".
fn imprimir_persona(persona: &Persona) {
    println!("Nombre: {}, Edad: {}", persona.nombre, persona.edad);
}

// Función principal del programa.
fn main() {
    // Creamos una instancia de la estructura "Persona" llamada "juan".
    let juan = crear_persona("Juan".to_string(), 25);

    // Imprimimos los datos de la instancia "juan".
    imprimir_persona(&juan);

    // Creamos una lista de instancias de la estructura "Persona".
    let personas: Vec<Persona> = vec![
        crear_persona("María".to_string(), 30),
        crear_persona("Pedro".to_string(), 40),
        crear_persona("Ana".to_string(), 50),
    ];

    // Iteramos sobre la lista de instancias de "Persona" e imprimimos los datos de cada una.
    for persona in &personas {
        imprimir_persona(persona);
    }
}
```

Explicación:

* La estructura "Persona" se define con dos campos: "nombre" y "edad".
* La función "crear_persona" crea una nueva instancia de la estructura "Persona" con los valores proporcionados como argumentos.
* La función "imprimir_persona" imprime los datos de una instancia de la estructura "Persona" en la consola.
* La función principal del programa, "main", crea una instancia de la estructura "Persona" llamada "juan" y la imprime en la consola.
* La función principal también crea una lista de instancias de la estructura "Persona" y la recorre para imprimir los datos de cada instancia en la consola.