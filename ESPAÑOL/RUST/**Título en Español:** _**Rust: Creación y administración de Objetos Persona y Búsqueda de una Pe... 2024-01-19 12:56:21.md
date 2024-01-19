```rust
// Definimos una estructura llamada "Persona" que representa una persona con nombre, edad y dirección.

#[derive(Debug)] // Con esto le decimos a Rust que queremos que se genere una implementación del trait Debug para nuestra estructura.
struct Persona {
    nombre: String,
    edad: i32,
    direccion: String,
}

// Definimos una función llamada "crear_persona" que toma un nombre, una edad y una dirección como argumentos y devuelve una nueva instancia de la estructura "Persona".

fn crear_persona(nombre: String, edad: i32, direccion: String) -> Persona {
    Persona {
        nombre,
        edad,
        direccion,
    }
}

// Definimos un vector llamado "personas" para almacenar una lista de personas.

let mut personas: Vec<Persona> = Vec::new();

// Creamos algunas personas usando la función "crear_persona" y las añadimos al vector "personas".

personas.push(crear_persona("Juan".to_string(), 25, "Calle Mayor 1".to_string()));
personas.push(crear_persona("María".to_string(), 30, "Calle Menor 2".to_string()));
personas.push(crear_persona("Pedro".to_string(), 35, "Calle Real 3".to_string()));

// Usamos un bucle "for" para iterar sobre el vector "personas" y mostrar la información de cada persona.

for persona in &personas {
    println!("Nombre: {}", persona.nombre);
    println!("Edad: {}", persona.edad);
    println!("Dirección: {}", persona.direccion);
    println!();
}

// Definimos una función llamada "encontrar_persona_por_nombre" que toma un nombre como argumento y devuelve la primera persona en el vector "personas" que tenga ese nombre.

fn encontrar_persona_por_nombre(nombre: &str) -> Option<&Persona> {
    for persona in &personas {
        if persona.nombre == nombre {
            return Some(persona);
        }
    }
    None
}

// Usamos la función "encontrar_persona_por_nombre" para buscar a una persona por su nombre.

let persona_buscada = encontrar_persona_por_nombre("María");

// Comprobamos si se ha encontrado a la persona.

if let Some(persona) = persona_buscada {
    println!("Persona encontrada:");
    println!("Nombre: {}", persona.nombre);
    println!("Edad: {}", persona.edad);
    println!("Dirección: {}", persona.direccion);
} else {
    println!("Persona no encontrada");
}
```

**Explicación del código:**

* El código crea una estructura llamada "Persona" que representa una persona con nombre, edad y dirección.
* El código crea una función llamada "crear_persona" que toma un nombre, una edad y una dirección como argumentos y devuelve una nueva instancia de la estructura "Persona".
* El código crea un vector llamado "personas" para almacenar una lista de personas.
* El código crea algunas personas usando la función "crear_persona" y las añade al vector "personas".
* El código usa un bucle "for" para iterar sobre el vector "personas" y mostrar la información de cada persona.
* El código define una función llamada "encontrar_persona_por_nombre" que toma un nombre como argumento y devuelve la primera persona en el vector "personas" que tenga ese nombre.
* El código usa la función "encontrar_persona_por_nombre" para buscar a una persona por su nombre.
* El código comprueba si se ha encontrado a la persona y, en caso afirmativo, muestra su información.