```rust
// Importamos las bibliotecas necesarias.
use std::io;
use std::collections::HashMap;

// Definimos una función para leer una línea de texto de la entrada estándar.
fn leer_linea() -> String {
    let mut linea = String::new();
    io::stdin().read_line(&mut linea).unwrap();
    linea.trim().to_string()
}

// Definimos una función para contar las palabras en una cadena de texto.
fn contar_palabras(texto: &str) -> HashMap<String, i32> {
    let mut palabras: HashMap<String, i32> = HashMap::new();
    for palabra in texto.split_whitespace() {
        let palabra_limpia = palabra.to_lowercase();
        *palabras.entry(palabra_limpia).or_insert(0) += 1;
    }
    palabras
}

// Definimos una función para imprimir las palabras y sus recuentos en orden descendente de recuento.
fn imprimir_palabras(palabras: &HashMap<String, i32>) {
    let mut palabras_ordenadas: Vec<(String, i32)> = palabras.iter().collect();
    palabras_ordenadas.sort_by(|a, b| b.1.cmp(&a.1));
    for (palabra, recuento) in palabras_ordenadas {
        println!("{}: {}", palabra, recuento);
    }
}

// Definimos la función principal del programa.
fn main() {
    // Leemos el texto de entrada del usuario.
    println!("Ingrese el texto:");
    let texto = leer_linea();

    // Contamos las palabras en el texto.
    let palabras = contar_palabras(&texto);

    // Imprimimos las palabras y sus recuentos.
    println!("Palabras y recuentos:");
    imprimir_palabras(&palabras);
}
```

**Explicación del código:**

* La función `leer_linea()` lee una línea de texto de la entrada estándar y la devuelve como una cadena de texto.
* La función `contar_palabras()` toma una cadena de texto como argumento y devuelve un `HashMap` que contiene las palabras en la cadena de texto como claves y el número de veces que aparecen como valores.
* La función `imprimir_palabras()` toma un `HashMap` de palabras y sus recuentos como argumento e imprime las palabras y sus recuentos en orden descendente de recuento.
* La función `main()` es la función principal del programa. Lee el texto de entrada del usuario, cuenta las palabras en el texto e imprime las palabras y sus recuentos.