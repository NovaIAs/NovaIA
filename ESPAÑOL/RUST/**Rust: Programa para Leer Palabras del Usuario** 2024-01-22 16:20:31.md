```rust
// Importando las bibliotecas necesarias
use std::io;

// Función principal del programa
fn main() {

    // Crear una instancia del objeto stdout
    let stdout = std::io::stdout();

    // Crear un objeto handle para el stdout
    let mut output = stdout.lock();

    // Crear un bucle infinito
    loop {
        // Mostrar un mensaje al usuario
        write!(output, "Introduce una palabra: ").unwrap();
        output.flush().unwrap();

        // Crear un objeto String para almacenar la entrada del usuario
        let mut input = String::new();

        // Leer la entrada del usuario
        io::stdin().read_line(&mut input).unwrap();

        // Eliminar los caracteres de nueva línea de la entrada
        input.pop();
    }
}
```

Este código es un programa en Rust que lee una palabra del usuario y la muestra en la pantalla. El programa utiliza el objeto `stdout` para mostrar el mensaje al usuario y el objeto `stdin` para leer la entrada del usuario. El bucle `loop` es un bucle infinito, lo que significa que el programa se ejecutará hasta que el usuario lo interrumpa. La función `write!` se utiliza para mostrar el mensaje al usuario, y la función `flush()` se utiliza para vaciar el búfer de salida. La función `read_line()` se utiliza para leer la entrada del usuario, y la función `pop()` se utiliza para eliminar los caracteres de nueva línea de la entrada.