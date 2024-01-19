```rust
// Una función que recibe una lista de números y devuelve el promedio.
fn promedio(numeros: &[f32]) -> f32 {
    // Si la lista está vacía, devolvemos 0.0.
    if numeros.is_empty() {
        return 0.0;
    }

    // Sumamos todos los números de la lista.
    let suma = numeros.iter().sum();

    // Dividimos la suma por el número de elementos de la lista.
    suma / numeros.len() as f32
}

// Un struct que representa una persona.
struct Persona {
    nombre: String,
    edad: u8,
    sexo: char,
}

// Un enum que representa los posibles estados de una persona.
enum Estado {
    Soltero,
    Casado,
    Viudo,
    Divorciado,
}

// Una función que devuelve una lista de personas.
fn crear_personas() -> Vec<Persona> {
    vec![
        Persona {
            nombre: "Juan".to_string(),
            edad: 20,
            sexo: 'M',
        },
        Persona {
            nombre: "María".to_string(),
            edad: 25,
            sexo: 'F',
        },
        Persona {
            nombre: "Pedro".to_string(),
            edad: 30,
            sexo: 'M',
        },
    ]
}

// Una función principal.
fn main() {
    // Creamos una lista de números.
    let numeros = vec![1.0, 2.0, 3.0, 4.0, 5.0];

    // Calculamos el promedio de la lista de números.
    let promedio_numeros = promedio(&numeros);

    // Creamos una lista de personas.
    let personas = crear_personas();

    // Imprimimos el promedio de la lista de números.
    println!("El promedio de los números es: {}", promedio_numeros);

    // Imprimimos la lista de personas.
    for persona in personas {
        println!("Nombre: {}, Edad: {}, Sexo: {}", persona.nombre, persona.edad, persona.sexo);
    }
}
```

Este código es una demostración de cómo crear una función, un struct, un enum y una función principal en Rust. La función `promedio` recibe una lista de números y devuelve el promedio. El struct `Persona` representa una persona con su nombre, edad y sexo. El enum `Estado` representa los posibles estados de una persona. La función `crear_personas` devuelve una lista de personas. La función principal crea una lista de números, calcula el promedio de la lista de números, crea una lista de personas e imprime el promedio de la lista de números y la lista de personas.