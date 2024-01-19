```rust
// Crear una estructura para almacenar los datos de una persona.
struct Persona {
    nombre: String,
    edad: u32,
    correo: String,
}

// Implementar un método para saludar a una persona.
impl Persona {
    fn saludar(&self) {
        println!("Hola, me llamo {} y tengo {} años.", self.nombre, self.edad);
    }
}

// Crear una función para crear una persona.
fn crear_persona(nombre: String, edad: u32, correo: String) -> Persona {
    Persona { nombre, edad, correo }
}

// Crear una lista de personas.
let personas = vec![
    crear_persona("Juan".to_string(), 25, "juan@ejemplo.com".to_string()),
    crear_persona("María".to_string(), 30, "maria@ejemplo.com".to_string()),
    crear_persona("Pedro".to_string(), 35, "pedro@ejemplo.com".to_string()),
];

// Recorrer la lista de personas y saludarlas.
for persona in &personas {
    persona.saludar();
}
```

Este código define una estructura `Persona` con tres campos: `nombre`, `edad` y `correo`.

Luego define un método `saludar` para la estructura `Persona` que imprime un mensaje de saludo con el nombre y la edad de la persona.

A continuación, define una función `crear_persona` que recibe tres argumentos: `nombre`, `edad` y `correo`, y devuelve una instancia de la estructura `Persona`.

Después, crea una lista de personas usando la función `crear_persona` y por último, recorre la lista de personas y llama al método `saludar` para cada persona.

Este código es complejo porque utiliza varias características avanzadas de Rust, como estructuras, métodos, funciones y genéricos. Sin embargo, el código es muy legible y fácil de entender, gracias al uso de comentarios y una sintaxis clara.