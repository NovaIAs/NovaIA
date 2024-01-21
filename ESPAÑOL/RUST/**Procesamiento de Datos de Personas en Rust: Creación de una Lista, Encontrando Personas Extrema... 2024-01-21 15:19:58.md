```rust
// Creamos una estructura para representar una persona
#[derive(Debug)]
struct Persona {
    nombre: String,
    edad: u32,
    ocupacion: String,
}

// Creamos una estructura para representar una lista de personas
#[derive(Debug)]
struct ListaDePersonas {
    lista: Vec<Persona>,
}

// Implementamos el método "agregar_persona" para la estructura "ListaDePersonas"
impl ListaDePersonas {
    fn agregar_persona(&mut self, persona: Persona) {
        self.lista.push(persona);
    }
}

// Creamos una función para crear una lista de personas
fn crear_lista_de_personas() -> ListaDePersonas {
    let mut lista = ListaDePersonas { lista: Vec::new() };

    lista.agregar_persona(Persona {
        nombre: "Juan".to_string(),
        edad: 20,
        ocupacion: "Estudiante".to_string(),
    });

    lista.agregar_persona(Persona {
        nombre: "María".to_string(),
        edad: 25,
        ocupacion: "Doctora".to_string(),
    });

    lista.agregar_persona(Persona {
        nombre: "Pedro".to_string(),
        edad: 30,
        ocupacion: "Ingeniero".to_string(),
    });

    lista
}

// Creamos una función para imprimir una lista de personas
fn imprimir_lista_de_personas(lista: &ListaDePersonas) {
    for persona in &lista.lista {
        println!("{:?}", persona);
    }
}

// Creamos una función para encontrar a la persona más joven en una lista de personas
fn encontrar_persona_mas_joven(lista: &ListaDePersonas) -> &Persona {
    let mut persona_mas_joven = &lista.lista[0];

    for persona in &lista.lista {
        if persona.edad < persona_mas_joven.edad {
            persona_mas_joven = persona;
        }
    }

    persona_mas_joven
}

// Creamos una función para encontrar a la persona más vieja en una lista de personas
fn encontrar_persona_mas_vieja(lista: &ListaDePersonas) -> &Persona {
    let mut persona_mas_vieja = &lista.lista[0];

    for persona in &lista.lista {
        if persona.edad > persona_mas_vieja.edad {
            persona_mas_vieja = persona;
        }
    }

    persona_mas_vieja
}

// Creamos una función para encontrar a la persona con la ocupación más común en una lista de personas
fn encontrar_ocupacion_mas_comun(lista: &ListaDePersonas) -> String {
    let mut ocupaciones: HashMap<String, u32> = HashMap::new();

    for persona in &lista.lista {
        let ocupacion = persona.ocupacion.clone();
        let contador = ocupaciones.entry(ocupacion).or_insert(0);
        *contador += 1;
    }

    let mut ocupacion_mas_comun = "";
    let mut contador_mas_alto = 0;

    for (ocupacion, contador) in &ocupaciones {
        if contador > contador_mas_alto {
            ocupacion_mas_comun = ocupacion;
            contador_mas_alto = contador;
        }
    }

    ocupacion_mas_comun.to_string()
}

// Creamos una función principal
fn main() {
    // Creamos una lista de personas
    let lista_de_personas = crear_lista_de_personas();

    // Imprimimos la lista de personas
    println!("Lista de personas:");
    imprimir_lista_de_personas(&lista_de_personas);

    // Encontramos a la persona más joven en la lista de personas
    let persona_mas_joven = encontrar_persona_mas_joven(&lista_de_personas);

    // Imprimimos a la persona más joven
    println!("Persona más joven:");
    println!("{:?}", persona_mas_joven);

    // Encontramos a la persona más vieja en la lista de personas
    let persona_mas_vieja = encontrar_persona_mas_vieja(&lista_de_personas);

    // Imprimimos a la persona más vieja
    println!("Persona más vieja:");
    println!("{:?}", persona_mas_vieja);

    // Encontramos a la persona con la ocupación más común en la lista de personas
    let ocupacion_mas_comun = encontrar_ocupacion_mas_comun(&lista_de_personas);

    // Imprimimos a la persona con la ocupación más común
    println!("Ocupación más común:");
    println!("{}", ocupacion_mas_comun);
}
```

**Explicación del código:**

1. Creamos una estructura llamada `Persona` que representa a una persona con los campos `nombre`, `edad` y `ocupacion`.
2. Creamos una estructura llamada `ListaDePersonas` que representa una lista de personas.
3. Implementamos el método `agregar_persona` para la estructura `ListaDePersonas` que permite agregar una persona a la lista.
4. Creamos una función llamada `crear_lista_de_personas` que crea una lista de personas con tres personas: Juan, María y Pedro.
5. Creamos una función llamada `imprimir_lista_de_personas` que imprime una lista de personas.
6. Creamos una función llamada `encontrar_persona_mas_joven` que encuentra a la persona más joven en una lista de personas.
7. Creamos una función llamada `encontrar_persona_mas_vieja` que encuentra a la persona más vieja en una lista de personas.
8. Creamos una función llamada `encontrar_ocupacion_mas_comun` que encuentra la ocupación más común en una lista de personas.
9. Creamos una función principal llamada `main` que crea una lista de personas, imprime la lista, encuentra a la persona más joven, encuentra a la persona más vieja y encuentra la ocupación más común.

Este código es un ejemplo de cómo se pueden usar estructuras, métodos, funciones y tipos en Rust para crear programas complejos.