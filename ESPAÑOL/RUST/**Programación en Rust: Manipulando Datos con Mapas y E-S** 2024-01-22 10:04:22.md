```rust
// Importar las bibliotecas necesarias
use std::io; // Para leer y escribir en la consola
use std::collections::HashMap; // Para crear un mapa de valores

// Definir una función para mostrar un mensaje en la consola
fn mostrar_mensaje(mensaje: &str) {
    println!("{}", mensaje);
}

// Definir una función para leer una línea de texto de la consola
fn leer_linea() -> String {
    let mut linea = String::new();
    io::stdin().read_line(&mut linea).expect("No se pudo leer la línea");
    linea
}

// Definir una función para crear un mapa de valores
fn crear_mapa() -> HashMap<String, String> {
    let mut mapa: HashMap<String, String> = HashMap::new();
    mapa.insert("nombre".to_string(), "Juan".to_string());
    mapa.insert("apellido".to_string(), "Pérez".to_string());
    mapa.insert("edad".to_string(), "25".to_string());
    mapa
}

// Definir una función para imprimir un mapa de valores
fn imprimir_mapa(mapa: &HashMap<String, String>) {
    for (clave, valor) in mapa {
        println!("{}: {}", clave, valor);
    }
}

// Definir una función principal
fn main() {
    // Mostrar un mensaje en la consola
    mostrar_mensaje("Hola, mundo!");

    // Leer una línea de texto de la consola
    let linea = leer_linea();

    // Crear un mapa de valores
    let mapa = crear_mapa();

    // Imprimir el mapa de valores
    imprimir_mapa(&mapa);
}
```

Este código es un ejemplo de código complejo en Rust. El código realiza las siguientes tareas:

* Muestra un mensaje en la consola.
* Lee una línea de texto de la consola.
* Crea un mapa de valores.
* Imprime el mapa de valores.

El código utiliza una variedad de características de Rust, incluyendo:

* Funciones para mostrar mensajes y leer líneas de texto.
* El tipo `HashMap` para crear un mapa de valores.
* El bucle `for` para iterar sobre el mapa.

Este código es un ejemplo de cómo se puede utilizar Rust para realizar tareas complejas de una manera concisa y eficiente.