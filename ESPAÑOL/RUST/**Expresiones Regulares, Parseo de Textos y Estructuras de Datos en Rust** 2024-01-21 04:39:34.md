```rust
// Expresiones regulares en Rust.

// Importamos la biblioteca `regex` para poder utilizar las expresiones regulares.
use regex::Regex;

// Definimos una expresión regular para buscar todas las apariciones de la palabra "hola" en una cadena de texto.
let re = Regex::new(r"hola").unwrap();

// Buscamos todas las apariciones de la expresión regular en la cadena de texto "Hola mundo, hola cómo estás?".
let matches = re.find_iter("Hola mundo, hola cómo estás?").collect::<Vec<_>>();

// Imprimimos las posiciones de todas las apariciones de la palabra "hola" en la cadena de texto.
for match in matches {
    println!("La palabra 'hola' aparece en la posición {}.", match.start());
}

// Implementamos un método `parse` para la estructura `Persona`. Este método recibe una cadena de texto y devuelve un objeto de tipo `Persona` si la cadena de texto tiene el formato correcto.
impl Persona {
    fn parse(texto: &str) -> Result<Persona, String> {
        // Definimos una expresión regular para validar el formato de la cadena de texto.
        let re = Regex::new(r"^([A-Za-z]+) ([A-Za-z]+) (\d+)$").unwrap();

        // Comprobamos si la cadena de texto tiene el formato correcto.
        if !re.is_match(texto) {
            return Err(format!("El formato de la cadena de texto '{}' no es válido.", texto));
        }

        // Extraemos el nombre, el apellido y la edad de la cadena de texto.
        let capturas = re.captures(texto).unwrap();
        let nombre = capturas.get(1).unwrap().as_str();
        let apellido = capturas.get(2).unwrap().as_str();
        let edad = capturas.get(3).unwrap().as_str().parse::<u32>().unwrap();

        // Creamos un objeto de tipo `Persona` con los datos extraídos de la cadena de texto.
        Ok(Persona {
            nombre: nombre.to_string(),
            apellido: apellido.to_string(),
            edad: edad,
        })
    }
}

// Definimos una función para leer los datos de un archivo de texto y crear una lista de objetos de tipo `Persona`.
fn leer_personas_desde_archivo(ruta_archivo: &str) -> Result<Vec<Persona>, String> {
    // Abrimos el archivo de texto para lectura.
    let archivo = std::fs::File::open(ruta_archivo)?;

    // Creamos un lector de líneas.
    let lector_lineas = std::io::BufReader::new(archivo);

    // Creamos una lista vacía para almacenar los objetos de tipo `Persona`.
    let mut personas: Vec<Persona> = Vec::new();

    // Leemos cada línea del archivo de texto y creamos un objeto de tipo `Persona` a partir de cada línea.
    for linea in lector_lineas.lines() {
        // Obtenemos la línea del archivo de texto.
        let linea = linea?;

        // Intentamos crear un objeto de tipo `Persona` a partir de la línea del archivo de texto.
        let persona = Persona::parse(&linea)?;

        // Añadimos el objeto de tipo `Persona` a la lista de personas.
        personas.push(persona);
    }

    // Devolvemos la lista de personas.
    Ok(personas)
}

// Definimos una función para imprimir los datos de una lista de objetos de tipo `Persona`.
fn imprimir_personas(personas: &Vec<Persona>) {
    // Imprimimos los datos de cada persona en la lista.
    for persona in personas {
        println!("Nombre: {} {}", persona.nombre, persona.apellido);
        println!("Edad: {}", persona.edad);
        println!("-----------------------------");
    }
}

// Función principal del programa.
fn main() {
    // Leemos los datos de las personas desde un archivo de texto.
    let personas = leer_personas_desde_archivo("personas.txt").unwrap();

    // Imprimimos los datos de las personas.
    imprimir_personas(&personas);
}
```

Este código es más complejo que el anterior y tiene varias características interesantes:

* Utiliza expresiones regulares para validar el formato de una cadena de texto.
* Define un método `parse` para la estructura `Persona` que permite crear un objeto de tipo `Persona` a partir de una cadena de texto.
* Define una función para leer los datos de un archivo de texto y crear una lista de objetos de tipo `Persona`.
* Define una función para imprimir los datos de una lista de objetos de tipo `Persona`.
* Utiliza la función `main` como punto de entrada del programa.

Este código es un ejemplo de cómo utilizar Rust para crear programas complejos que interactúan con archivos de texto y estructuras de datos.