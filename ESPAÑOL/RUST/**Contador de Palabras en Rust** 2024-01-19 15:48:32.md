```rust
// Importar las bibliotecas necesarias
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

// Definir una función para leer un archivo de texto y retornar su contenido como una cadena de caracteres.
fn leer_archivo(ruta_archivo: &str) -> Result<String, std::io::Error> {
    // Abrir el archivo en modo de lectura.
    let archivo = File::open(ruta_archivo)?;

    // Crear un "buffer" para leer el archivo de forma más eficiente.
    let buffer = BufReader::new(archivo);

    // Leer el archivo línea por línea y acumular el contenido en una cadena de caracteres.
    let mut contenido = String::new();
    for linea in buffer.lines() {
        let linea_str = linea?;
        contenido.push_str(&linea_str);
        contenido.push('\n');  // Añadir un salto de línea después de cada línea.
    }

    // Retornar el contenido del archivo como una cadena de caracteres.
    Ok(contenido)
}

// Definir una función para contar la frecuencia de las palabras en una cadena de caracteres.
fn contar_palabras(texto: &str) -> HashMap<String, usize> {
    // Crear un "HashMap" para almacenar la frecuencia de las palabras.
    let mut frecuencias: HashMap<String, usize> = HashMap::new();

    // Dividir el texto en palabras usando el carácter de espacio como delimitador.
    let palabras = texto.split(' ');

    // Iterar sobre las palabras y actualizar su frecuencia en el "HashMap".
    for palabra in palabras {
        // Convertir la palabra a minúsculas para ignorar la capitalización.
        let palabra_minuscula = palabra.to_lowercase();

        // Obtener la frecuencia actual de la palabra.
        let frecuencia_actual = frecuencias.get(&palabra_minuscula).unwrap_or(&0);

        // Incrementar la frecuencia de la palabra en una unidad.
        let nueva_frecuencia = frecuencia_actual + 1;

        // Insertar la frecuencia actualizada en el "HashMap".
        frecuencias.insert(palabra_minuscula, nueva_frecuencia);
    }

    // Retornar el "HashMap" con las frecuencias de las palabras.
    frecuencias
}

// Definir una función para imprimir las frecuencias de las palabras en orden descendente.
fn imprimir_frecuencias(frecuencias: &HashMap<String, usize>) {
    // Convertir el "HashMap" en una lista de tuplas (palabra, frecuencia).
    let lista_frecuencias: Vec<(String, usize)> = frecuencias.iter().collect();

    // Ordenar la lista de tuplas en orden descendente basado en la frecuencia.
    let lista_ordenada = lista_frecuencias.sort_by(|a, b| b.1.cmp(&a.1));

    // Imprimir las tuplas ordenadas, mostrando la palabra y su frecuencia.
    for (palabra, frecuencia) in lista_ordenada {
        println!("{}: {}", palabra, frecuencia);
    }
}

// Función principal del programa.
fn main() {

    // Obtener la ruta del archivo de texto desde la línea de comandos.
    let ruta_archivo = std::env::args().nth(1).expect("Debes especificar la ruta del archivo de texto.");


    // Leer el contenido del archivo de texto.
    let contenido = leer_archivo(&ruta_archivo).expect("No se pudo leer el archivo de texto.");

    // Contar la frecuencia de las palabras en el contenido del archivo.
    let frecuencias = contar_palabras(&contenido);

    // Imprimir las frecuencias de las palabras en orden descendente.
    imprimir_frecuencias(&frecuencias);
}
```

**Explicación del código:**

* La función `leer_archivo()` lee el contenido de un archivo de texto y lo retorna como una cadena de caracteres.
* La función `contar_palabras()` cuenta la frecuencia de las palabras en una cadena de caracteres y retorna un "HashMap" con las palabras como claves y sus frecuencias como valores.
* La función `imprimir_frecuencias()` imprime las frecuencias de las palabras en orden descendente.
* La función `main()` es la función principal del programa. Obtiene la ruta del archivo de texto desde la línea de comandos, llama a las funciones `leer_archivo()`, `contar_palabras()` y `imprimir_frecuencias()` para procesar el archivo y mostrar las frecuencias de las palabras.