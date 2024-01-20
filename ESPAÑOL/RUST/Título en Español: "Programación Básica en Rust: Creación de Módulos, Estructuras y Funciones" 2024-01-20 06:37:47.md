```rust
// Modulo Principal del programa
mod principal {

    // Constantes
    const MENSAJE: &str = "Hola Mundo!";

    // Estructura
    struct Persona {
        nombre: String,
        edad: u16,
    }

    // Funciones
    fn saludar() {
        println!("{}", MENSAJE);
    }

    fn crear_persona(nombre: String, edad: u16) -> Persona {
        Persona { nombre, edad }
    }

    // Función principal
    fn main() {
        // Llamada a las funciones
        saludar();

        // Crear una instancia de la estructura Persona
        let persona = crear_persona("Juan".to_string(), 25);

        // Imprimir la información de la persona
        println!("Nombre: {}, Edad: {}", persona.nombre, persona.edad);
    }
}

// Modulo Adicional
mod adicional {

    // Constantes
    const MENSAJE_ADICIONAL: &str = "Mensaje adicional";

    // Función
    fn imprimir_mensaje_adicional() {
        println!("{}", MENSAJE_ADICIONAL);
    }

    // Función principal del módulo adicional
    pub fn main() {
        imprimir_mensaje_adicional();
    }
}

// Punto de entrada al programa
fn main() {
    // Llamada a la función principal del módulo principal
    principal::main();

    // Llamada a la función principal del módulo adicional
    adicional::main();
}
```

Explicación:

* El código está escrito en el lenguaje de programación Rust.
* El código está dividido en módulos: `principal` y `adicional`.
* El módulo `principal` contiene la función principal `main()` del programa, así como otras funciones y estructuras.
* El módulo `adicional` contiene una función principal `main()` y otra función que imprime un mensaje adicional.
* El código utiliza constantes, estructuras y funciones.
* El código utiliza el sistema de módulos de Rust para organizar el código en módulos separados.
* El código también utiliza el tipo `String` de Rust para representar cadenas de texto.