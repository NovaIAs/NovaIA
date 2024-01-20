```rust
use std::io;

fn main() {
    let mut input = String::new();

    // Lee la entrada del usuario
    println!("Ingrese una cadena de texto:");
    io::stdin().read_line(&mut input).expect("Error al leer la entrada");

    // Elimina los espacios en blanco de la cadena
    let input = input.trim();

    // Convierte la cadena a minúsculas
    let input = input.to_lowercase();

    // Inicializa un mapa para almacenar los caracteres y sus frecuencias
    let mut char_freq_map: HashMap<char, usize> = HashMap::new();

    // Itera sobre la cadena y actualiza el mapa de frecuencias de caracteres
    for c in input.chars() {
        let count = char_freq_map.entry(c).or_insert(0);
        *count += 1;
    }

    // Ordena el mapa de frecuencias de caracteres por valor en orden descendente
    let mut sorted_char_freq_map: Vec<(char, usize)> = char_freq_map.into_iter().collect();
    sorted_char_freq_map.sort_by(|a, b| b.1.cmp(&a.1));

    // Imprime los caracteres y sus frecuencias en orden descendente
    println!("Caracteres y frecuencias:");
    for (c, f) in sorted_char_freq_map {
        println!("{}: {}", c, f);
    }
}
```

Explicación:

1. **Lee la entrada del usuario:** Se solicita al usuario que ingrese una cadena de texto y se lee utilizando la función `read_line` de `io::stdin()`.

2. **Elimina los espacios en blanco de la cadena:** Se eliminan los espacios en blanco de la cadena ingresada utilizando el método `trim()`.

3. **Convierte la cadena a minúsculas:** Se convierte la cadena a minúsculas utilizando el método `to_lowercase()`. Esto se hace para que los caracteres no sean sensibles a mayúsculas y minúsculas al contar sus frecuencias.

4. **Inicializa un mapa para almacenar los caracteres y sus frecuencias:** Se crea un mapa utilizando la estructura de datos `HashMap` proporcionada por la biblioteca estándar de Rust para almacenar los caracteres y sus frecuencias. El tipo de clave es `char` y el tipo de valor es `usize`, que se utiliza para almacenar la frecuencia de cada carácter.

5. **Itera sobre la cadena y actualiza el mapa de frecuencias de caracteres:** Se itera sobre la cadena y se actualiza el mapa de frecuencias de caracteres utilizando el método `entry()`. Si un carácter ya existe en el mapa, se incrementa su frecuencia. Si no existe, se añade al mapa con una frecuencia de 1.

6. **Ordena el mapa de frecuencias de caracteres por valor en orden descendente:** Se ordena el mapa de frecuencias de caracteres por valor en orden descendente utilizando el método `sort_by()` y el comparador `cmp()` proporcionado por `std::cmp`. Esto clasifica los caracteres por su frecuencia de aparición en la cadena.

7. **Imprime los caracteres y sus frecuencias en orden descendente:** Se imprimen los caracteres y sus frecuencias en orden descendente utilizando un bucle `for`.