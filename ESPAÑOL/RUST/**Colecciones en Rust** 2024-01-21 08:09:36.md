```rust
// Módulo principal del programa
mod main {

    // Función principal del programa
    fn main() {
        // Crear una lista de números
        let numeros = vec![1, 2, 3, 4, 5];

        // Crear una lista de cadenas
        let cadenas = vec!["Hola", "Mundo", "!"];

        // Crear una lista de tuplas
        let tuplas = vec![
            (1, "Hola"),
            (2, "Mundo"),
            (3, "!"),
        ];

        // Crear una lista de estructuras
        struct Persona {
            nombre: String,
            edad: u8,
        }

        let personas = vec![
            Persona {
                nombre: "Juan",
                edad: 20,
            },
            Persona {
                nombre: "María",
                edad: 25,
            },
            Persona {
                nombre: "Pedro",
                edad: 30,
            },
        ];

        // Iterar sobre la lista de números
        for numero in numeros.iter() {
            println!("Número: {}", numero);
        }

        // Iterar sobre la lista de cadenas
        for cadena in cadenas.iter() {
            println!("Cadena: {}", cadena);
        }

        // Iterar sobre la lista de tuplas
        for tupla in tuplas.iter() {
            println!("Tupla: {:?}", tupla);
        }

        // Iterar sobre la lista de estructuras
        for persona in personas.iter() {
            println!("Persona: {:?}", persona);
        }

        // Crear un mapa de claves y valores
        let mapa = std::collections::HashMap::new();
        mapa.insert("Nombre", "Juan");
        mapa.insert("Apellido", "Pérez");
        mapa.insert("Edad", 20);

        // Iterar sobre el mapa
        for (clave, valor) in mapa.iter() {
            println!("Clave: {}, Valor: {}", clave, valor);
        }
    }
}
```

Explicación:

* El código crea una variedad de listas, incluyendo listas de números, cadenas, tuplas y estructuras.
* También crea un mapa de claves y valores.
* El código luego itera sobre cada uno de estos contenedores, imprimiendo su contenido en la consola.
* Este código es un ejemplo de cómo trabajar con diferentes tipos de contenedores en Rust.