```rust
// Definir una estructura para almacenar información de una persona.
#[derive(Debug)]
struct Persona {
    nombre: String,
    edad: u8,
    ciudad: String,
}

// Implementar un método para saludar a una persona.
impl Persona {
    fn saludar(&self) {
        println!("Hola, mi nombre es {} y tengo {} años. Soy de {}.", self.nombre, self.edad, self.ciudad);
    }
}

// Definir una función para crear una lista de personas.
fn crear_lista_de_personas() -> Vec<Persona> {
    // Crear una lista vacía.
    let mut lista: Vec<Persona> = Vec::new();

    // Agregar algunas personas a la lista.
    lista.push(Persona { nombre: "Juan".to_string(), edad: 20, ciudad: "Madrid".to_string() });
    lista.push(Persona { nombre: "María".to_string(), edad: 25, ciudad: "Barcelona".to_string() });
    lista.push(Persona { nombre: "Pedro".to_string(), edad: 30, ciudad: "Valencia".to_string() });

    // Devolver la lista.
    lista
}

// Definir una función para imprimir la información de una lista de personas.
fn imprimir_lista_de_personas(lista: &Vec<Persona>) {
    // Recorrer la lista y llamar al método saludar de cada persona.
    for persona in lista {
        persona.saludar();
    }
}

// Definir una función principal.
fn main() {
    // Crear una lista de personas.
    let lista_de_personas = crear_lista_de_personas();

    // Imprimir la información de la lista de personas.
    imprimir_lista_de_personas(&lista_de_personas);
}
```

Este código crea una estructura llamada Persona que tiene tres campos: nombre, edad y ciudad. Luego implementa un método llamado saludar que imprime un mensaje de saludo usando los valores de los campos de la persona. A continuación, define una función llamada crear_lista_de_personas que devuelve una lista de personas. Luego, define una función llamada imprimir_lista_de_personas que imprime la información de una lista de personas. Finalmente, define una función principal que crea una lista de personas y luego la imprime.

Este código es complejo porque utiliza varias características de Rust, incluyendo estructuras, métodos, funciones y listas. También utiliza la sintaxis de Rust, que puede ser difícil de entender para los principiantes. Sin embargo, el código está bien documentado y es fácil de seguir si se tiene un conocimiento básico de Rust.