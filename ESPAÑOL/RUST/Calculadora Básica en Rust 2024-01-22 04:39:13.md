```rust
// Importamos la biblioteca estándar de Rust.
use std::io;

// Creamos una función principal, que será el punto de entrada de nuestro programa.
fn main() {
    // Pedimos al usuario que introduzca un número.
    println!("Introduce un número:");

    // Creamos una variable para almacenar el número introducido por el usuario.
    let mut numero = String::new();

    // Leemos el número introducido por el usuario.
    io::stdin().read_line(&mut numero).expect("Error al leer el número.");

    // Convertimos el número introducido por el usuario a un tipo de dato entero de 32 bits.
    let numero = numero.trim().parse::<i32>().expect("Error al convertir el número a entero.");

    // Creamos un bucle infinito.
    loop {
        // Mostramos el número introducido por el usuario.
        println!("El número introducido es: {}", numero);

        // Pedimos al usuario que introduzca una operación.
        println!("Introduce una operación(+, -, *, /):");

        // Creamos una variable para almacenar la operación introducida por el usuario.
        let mut operacion = String::new();

        // Leemos la operación introducida por el usuario.
        io::stdin().read_line(&mut operacion).expect("Error al leer la operación.");

        // Eliminamos los espacios en blanco de la operación introducida por el usuario.
        let operacion = operacion.trim();

        // Creamos una variable para almacenar el resultado de la operación.
        let mut resultado = 0;

        // Comprobamos la operación introducida por el usuario.
        match operacion {
            "+" => resultado = numero + numero,
            "-" => resultado = numero - numero,
            "*" => resultado = numero * numero,
            "/" => {
                if numero == 0 {
                    println!("Error: no se puede dividir por cero.");
                    continue;
                } else {
                    resultado = numero / numero;
                }
            },
            _ => println!("Error: operación no válida."),
        }

        // Mostramos el resultado de la operación.
        println!("El resultado de la operación es: {}", resultado);

        // Preguntamos al usuario si desea continuar.
        println!("¿Desea continuar? (s/n):");

        // Creamos una variable para almacenar la respuesta del usuario.
        let mut respuesta = String::new();

        // Leemos la respuesta del usuario.
        io::stdin().read_line(&mut respuesta).expect("Error al leer la respuesta.");

        // Eliminamos los espacios en blanco de la respuesta del usuario.
        let respuesta = respuesta.trim();

        // Comprobamos la respuesta del usuario.
        if respuesta == "n" {
            break;
        }
    }
}
```

Este código es un programa que pide al usuario que introduzca un número y una operación, y luego calcula el resultado de la operación. El programa utiliza un bucle infinito para permitir al usuario realizar múltiples operaciones.

El código está dividido en varias funciones, cada una de las cuales realiza una tarea específica. La función `main` es el punto de entrada del programa y llama a las otras funciones para realizar las operaciones.

La función `pedir_numero` pide al usuario que introduzca un número y lo devuelve como un tipo de dato entero de 32 bits.

La función `pedir_operacion` pide al usuario que introduzca una operación y la devuelve como una cadena de caracteres.

La función `calcular_resultado` calcula el resultado de la operación utilizando la función `match`, que comprueba la operación introducida por el usuario y realiza la operación correspondiente.

La función `mostrar_resultado` muestra el resultado de la operación en la consola.

La función `preguntar_continuar` pregunta al usuario si desea continuar y devuelve la respuesta del usuario como una cadena de caracteres.

El programa utiliza un bucle infinito para permitir al usuario realizar múltiples operaciones. El bucle se rompe cuando el usuario introduce la letra "n" en respuesta a la pregunta "¿Desea continuar?".