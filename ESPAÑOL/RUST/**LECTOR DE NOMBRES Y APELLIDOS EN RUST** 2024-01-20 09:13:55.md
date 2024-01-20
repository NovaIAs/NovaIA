```rust
use std::collections::HashMap;
use std::io::{self, BufRead};

fn main() {
    let mut mapa: HashMap<String, Vec<String>> = HashMap::new();

    // Leer nombres y apellidos de un archivo de texto
    let mut reader = io::BufReader::new(io::stdin());
    let mut line = String::new();
    while reader.read_line(&mut line).unwrap() > 0 {
        // Dividir la línea por espacios
        let palabras: Vec<&str> = line.split(' ').collect();

        // Obtener el nombre y el apellido
        let nombre = palabras[0];
        let apellido = palabras[1];

        // Agregar el apellido al mapa
        if mapa.contains_key(nombre) {
            mapa.get_mut(nombre).unwrap().push(apellido);
        } else {
            mapa.insert(nombre, vec![apellido]);
        }

        // Limpiar la variable `line`
        line.clear();
    }

    // Imprimir el mapa
    for (nombre, apellidos) in &mapa {
        println!("{}:", nombre);
        for apellido in apellidos {
            println!("  - {}", apellido);
        }
    }
}
```

**Explicación:**

Este código en Rust crea un mapa que asocia nombres con una lista de apellidos. El mapa se crea leyendo líneas de un archivo de texto. Cada línea contiene un nombre y un apellido, separados por un espacio.

La función `main()` crea una variable `mapa` de tipo `HashMap<String, Vec<String>>`. `HashMap` es una estructura de datos que almacena pares clave-valor. En este caso, la clave es un nombre y el valor es una lista de apellidos.

Luego, el código lee líneas del archivo de texto utilizando la variable `reader`. Cada línea se divide en palabras usando el método `split()`. Las palabras se almacenan en un vector llamado `palabras`.

El nombre y el apellido se extraen del vector `palabras` y se agregan al mapa. Si el nombre ya existe en el mapa, el apellido se agrega a la lista de apellidos asociada con ese nombre. De lo contrario, se crea una nueva entrada en el mapa con el nombre como clave y una lista de apellidos que contiene el apellido.

Finalmente, el mapa se imprime en la consola utilizando un bucle `for`. Para cada clave-valor del mapa, se imprime el nombre y los apellidos asociados.