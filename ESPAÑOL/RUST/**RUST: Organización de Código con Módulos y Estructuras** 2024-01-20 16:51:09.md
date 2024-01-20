```rust
// Creando un módulo para organizar el código
mod mi_modulo {
    // Creando una función para saludar a alguien
    pub fn saludar(nombre: &str) {
        println!("¡Hola, {}!", nombre);
    }

    // Creando una estructura para representar a una persona
    pub struct Persona {
        nombre: String,
        edad: u8,
    }

    // Creando una implementación de la estructura Persona
    impl Persona {
        // Creando un método para obtener el nombre de la persona
        pub fn obtener_nombre(&self) -> &str {
            &self.nombre
        }

        // Creando un método para obtener la edad de la persona
        pub fn obtener_edad(&self) -> u8 {
            self.edad
        }
    }
}

// Utilizando el módulo creado
use mi_modulo::Persona;

// Creando una función principal
fn main() {

    // Creando una instancia de la estructura Persona
    let persona = Persona {
        nombre: String::from("Juan"),
        edad: 25,
    };

    // Utilizando los métodos de la estructura Persona
    println!("Nombre: {}", persona.obtener_nombre());
    println!("Edad: {}", persona.obtener_edad());

    // Utilizando la función saludar del módulo
    mi_modulo::saludar("María");
}
```

Explicación:

1. **Módulo Mi_Módulo:** 
Este módulo se crea para organizar el código y agrupar funciones y estructuras relacionadas. 


2. **Función Saludar:** 
Esta función toma un parámetro de tipo cadena de caracteres y muestra un mensaje de saludo. Se declara como `pub` para indicar que puede ser utilizada desde fuera del módulo. 


3. **Estructura Persona:** 
Esta estructura se define para representar a una persona y tiene dos campos: `nombre` y `edad`. También se declara como `pub` para que pueda ser utilizada desde fuera del módulo. 


4. **Implementación de la Estructura Persona:** 
Esta sección contiene los métodos de la estructura Persona. Los métodos se utilizan para obtener el nombre y la edad de la persona. 


5. **Utilizando el Módulo Creado:** 
La línea `use mi_modulo::Persona;` importa la estructura Persona del módulo mi_modulo al alcance actual. 


6. **Función Principal Main:** 
Esta es la función de entrada del programa. 


7. **Creando una Instancia de la Estructura Persona:** 
La línea `let persona = Persona { nombre: String::from("Juan"), edad: 25 };` crea una instancia de la estructura Persona y la asigna a la variable `persona`. 


8. **Utilizando los Métodos de la Estructura Persona:** 
Las líneas `println!("Nombre: {}", persona.obtener_nombre());` e `println!("Edad: {}", persona.obtener_edad());` utilizan los métodos `obtener_nombre` y `obtener_edad` de la estructura Persona para obtener el nombre y la edad de la persona, respectivamente.


9. **Utilizando la Función Saludar del Módulo:** 
La línea `mi_modulo::saludar("María");` llama a la función `saludar` del módulo `mi_modulo` y le pasa el nombre "María" como argumento.