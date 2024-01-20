```rust
// Importar las librerías necesarias
use std::io;
use std::collections::HashMap;

// Definir la función principal
fn main() {
    // Crear un HashMap para almacenar los datos
    let mut datos: HashMap<String, Vec<String>> = HashMap::new();

    // Crear un loop para obtener los datos del usuario
    loop {
        // Obtener el nombre del usuario
        println!("¿Cómo te llamas?");
        let mut nombre = String::new();
        io::stdin().read_line(&mut nombre).unwrap();

        // Obtener los datos del usuario
        println!("¿Cuáles son tus datos?");
        let mut datos_usuario = String::new();
        io::stdin().read_line(&mut datos_usuario).unwrap();

        // Dividir los datos del usuario en una lista
        let datos_usuario: Vec<String> = datos_usuario.split(",").map(|x| x.trim().to_string()).collect();

        // Agregar los datos del usuario al HashMap
        datos.insert(nombre, datos_usuario);

        // Preguntar al usuario si quiere continuar
        println!("¿Quieres continuar? (s/n)");
        let mut continuar = String::new();
        io::stdin().read_line(&mut continuar).unwrap();

        // Si el usuario no quiere continuar, salir del loop
        if continuar.trim() == "n" {
            break;
        }
    }

    // Imprimir los datos del usuario
    println!("Datos del usuario:");
    for (nombre, datos_usuario) in &datos {
        println!("Nombre: {}", nombre);
        println!("Datos: {:?}", datos_usuario);
    }
}
```

Explicación del código:

* La función `main()` es la función principal del programa y se ejecuta cuando se ejecuta el programa.
* La variable `datos` es un HashMap que se utiliza para almacenar los datos del usuario.
* El loop infinito se utiliza para obtener los datos del usuario hasta que el usuario no quiera continuar.
* La variable `nombre` se utiliza para almacenar el nombre del usuario.
* La variable `datos_usuario` se utiliza para almacenar los datos del usuario.
* La función `split()` se utiliza para dividir los datos del usuario en una lista.
* La función `trim()` se utiliza para eliminar los espacios en blanco de los elementos de la lista.
* La función `insert()` se utiliza para agregar los datos del usuario al HashMap.
* La función `read_line()` se utiliza para obtener una línea de texto del usuario.
* La función `println()` se utiliza para imprimir texto en la consola.