```rust
// Bibliotecas necesarias
use std::io;
use std::path::Path;
use std::fs;
use std::collections::HashMap;
use std::error::Error;

// Función para leer un archivo de texto y devolver su contenido como una cadena
fn leer_archivo(ruta: &Path) -> Result<String, Box<dyn Error>> {
    let contenido = fs::read_to_string(ruta)?;
    Ok(contenido)
}

// Función para contar el número de ocurrencias de cada palabra en una cadena
fn contar_palabras(contenido: &str) -> HashMap<String, usize> {
    let mut palabras = HashMap::new();

    for palabra in contenido.split_whitespace() {
        let palabra_limpia = palabra.to_lowercase().replace(".", "").replace(",", "");

        if palabra_limpia != "" {
            *palabras.entry(palabra_limpia).or_insert(0) += 1;
        }
    }

    palabras
}

// Función para imprimir las palabras y sus ocurrencias en orden descendente
fn imprimir_palabras(palabras: &HashMap<String, usize>) {
    let mut palabras_ordenadas: Vec<(String, usize)> = palabras.iter().collect();
    palabras_ordenadas.sort_by(|a, b| b.1.cmp(&a.1));

    for (palabra, ocurrencias) in palabras_ordenadas {
        println!("{}: {}", palabra, ocurrencias);
    }
}

// Función principal
fn main() {
    // 1. Leer el archivo de texto
    let ruta = Path::new("texto.txt");
    let contenido = leer_archivo(&ruta).expect("Error al leer el archivo");

    // 2. Contar el número de ocurrencias de cada palabra
    let palabras = contar_palabras(&contenido);

    // 3. Imprimir las palabras y sus ocurrencias en orden descendente
    imprimir_palabras(&palabras);
}
```

Este código es un programa completo que lee un archivo de texto, cuenta el número de ocurrencias de cada palabra en el archivo y luego imprime las palabras y sus ocurrencias en orden descendente.

El código se divide en tres funciones principales:

1. `leer_archivo` lee el contenido de un archivo de texto y lo devuelve como una cadena
2. `contar_palabras` cuenta el número de ocurrencias de cada palabra en una cadena
3. `imprimir_palabras` imprime las palabras y sus ocurrencias en orden descendente

La función `main()` es la función principal del programa. Esta función llama a las otras tres funciones para leer el archivo de texto, contar el número de ocurrencias de cada palabra y luego imprimir las palabras y sus ocurrencias en orden descendente.

El código también utiliza una biblioteca llamada `HashMap` para almacenar el número de ocurrencias de cada palabra. Un `HashMap` es una estructura de datos que almacena pares clave-valor, donde la clave es la palabra y el valor es el número de ocurrencias.

El código también utiliza una función llamada `sort_by()` para ordenar los pares clave-valor en orden descendente. Esto se hace para que las palabras con más ocurrencias se impriman primero.

Finalmente, el código utiliza la función `println!()` para imprimir las palabras y sus ocurrencias en orden descendente.