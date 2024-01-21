```rust
// Código complejo en Rust

// Definir una estructura para representar una persona
#[derive(Debug)]
struct Persona {
    nombre: String,
    edad: u8,
    genero: String,
    ocupacion: String,
}

// Definir una función para crear una persona
fn crear_persona(nombre: String, edad: u8, genero: String, ocupacion: String) -> Persona {
    Persona {
        nombre: nombre,
        edad: edad,
        genero: genero,
        ocupacion: ocupacion,
    }
}

// Definir una función para imprimir la información de una persona
fn imprimir_persona(persona: &Persona) {
    println!("Nombre: {}", persona.nombre);
    println!("Edad: {}", persona.edad);
    println!("Género: {}", persona.genero);
    println!("Ocupación: {}", persona.ocupacion);
}

// Crear una lista de personas
let personas: Vec<Persona> = vec![
    crear_persona("Juan", 25, "Masculino", "Ingeniero"),
    crear_persona("María", 30, "Femenino", "Doctora"),
    crear_persona("Pedro", 35, "Masculino", "Abogado"),
    crear_persona("Ana", 40, "Femenino", "Profesora"),
];

// Iterar sobre la lista de personas e imprimir la información de cada una
for persona in personas.iter() {
    imprimir_persona(persona);
    println!();
}

// Definir una función para encontrar a la persona más vieja
fn encontrar_persona_mas_vieja(personas: &[Persona]) -> &Persona {
    let mut persona_mas_vieja = &personas[0];

    for persona in personas.iter() {
        if persona.edad > persona_mas_vieja.edad {
            persona_mas_vieja = persona;
        }
    }

    persona_mas_vieja
}

// Imprimir la información de la persona más vieja
println!("La persona más vieja es:");
imprimir_persona(encontrar_persona_mas_vieja(&personas));

// Definir una función para encontrar a la persona más joven
fn encontrar_persona_mas_joven(personas: &[Persona]) -> &Persona {
    let mut persona_mas_joven = &personas[0];

    for persona in personas.iter() {
        if