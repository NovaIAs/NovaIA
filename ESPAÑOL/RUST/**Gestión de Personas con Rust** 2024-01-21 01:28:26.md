```rust
// Importamos las librerías necesarias
use std::io;
use std::collections::HashMap;

// Definimos una función para leer una línea de texto de la consola
fn leer_linea() -> String {
    let mut linea = String::new();
    io::stdin().read_line(&mut linea).expect("Error al leer la línea");
    linea
}

// Definimos una función para crear un mapa con los datos de una persona
fn crear_persona(nombre: String, edad: u32) -> HashMap<String, u32> {
    let mut persona = HashMap::new();
    persona.insert(nombre, edad);
    persona
}

// Definimos una función para imprimir los datos de una persona
fn imprimir_persona(persona: &HashMap<String, u32>) {
    for (nombre, edad) in persona {
        println!("Nombre: {}, Edad: {}", nombre, edad);
    }
}

// Definimos una función para agregar una persona a una lista
fn agregar_persona(lista_personas: &mut Vec<HashMap<String, u32>>, persona: HashMap<String, u32>) {
    lista_personas.push(persona);
}

// Definimos una función para buscar una persona en una lista
fn buscar_persona(lista_personas: &Vec<HashMap<String, u32>>, nombre: &String) -> Option<HashMap<String, u32>> {
    for persona in lista_personas {
        if persona.contains_key(nombre) {
            return Some(persona.clone());
        }
    }
    None
}

// Definimos una función para eliminar una persona de una lista
fn eliminar_persona(lista_personas: &mut Vec<HashMap<String, u32>>, nombre: &String) {
    let mut indice = 0;
    for persona in lista_personas {
        if persona.contains_key(nombre) {
            break;
        }
        indice += 1;
    }
    lista_personas.remove(indice);
}

// Definimos una función para imprimir la lista de personas
fn imprimir_lista_personas(lista_personas: &Vec<HashMap<String, u32>>) {
    for persona in lista_personas {
        imprimir_persona(persona);
    }
}

// Definimos una función para el menú principal
fn menu_principal() {
    println!("¿Qué desea hacer?");
    println("1. Agregar una persona");
    println("2. Buscar una persona");
    println("3. Eliminar una persona");
    println("4. Imprimir la lista de personas");
    println("5. Salir");
    let opcion = leer_linea();
    match opcion.trim() {
        "1" => agregar_persona(),
        "2" => buscar_persona(),
        "3" => eliminar_persona(),
        "4" => imprimir_lista_personas(),
        "5" => println!("¡Hasta luego!"),
        _ => println!("Opción no válida"),
    }
}

// Definimos la función main
fn main() {
    // Creamos una lista de personas
    let mut lista_personas = Vec::new();

    // Agregamos algunas personas a la lista
    agregar_persona(&mut lista_personas, crear_persona("Juan".to_string(), 20));
    agregar_persona(&mut lista_personas, crear_persona("María".to_string(), 25));
    agregar_persona(&mut lista_personas, crear_persona("Pedro".to_string(), 30));

    // Imprimimos la lista de personas
    imprimir_lista_personas(&lista_personas);

    // Llamamos al menú principal
    menu_principal();
}
```

Este código crea una lista de personas y permite al usuario interactuar con ella. El usuario puede agregar, buscar, eliminar o imprimir la lista de personas. El código está bien estructurado y utiliza funciones para organizar el código y hacerlo más legible.