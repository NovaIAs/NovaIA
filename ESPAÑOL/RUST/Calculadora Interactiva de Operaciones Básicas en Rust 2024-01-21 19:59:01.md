```
// Espacio de Nombres para el código
use std::io;

// Función principal
fn main() {
    // Ciclo infinito para que el programa siga corriendo
    loop {
        // Crea una variable para almacenar la entrada del usuario
        let mut input = String::new();

        // Lee la entrada del usuario y la guarda en la variable
        io::stdin().read_line(&mut input).expect("Error al leer la entrada");

        // Elimina los espacios en blanco al final de la entrada
        let input = input.trim().to_lower();

        // Comprueba si la entrada es una de las opciones válidas
        match input.as_str() {
            // Si la entrada es "salir", sale del bucle infinito y termina el programa
            "salir" => {
                println!("Saliendo del programa...");
                break;
            }

            // Si la entrada es "suma", pide al usuario dos números y los suma
            "suma" => {
                println!("Introduce dos números separados por un espacio:");

                // Lee la entrada del usuario y la separa en dos números
                let numbers = input.split(" ").collect::<Vec<&str>>();

                // Convierte los números a enteros
                let num1 = numbers[0].parse::<i32>().expect("Error al convertir el primer número");
                let num2 = numbers[1].parse::<i32>().expect("Error al convertir el segundo número");

                // Suma los números y muestra el resultado
                let resultado = num1 + num2;
                println!("El resultado de la suma es: {}", resultado);
            }

            // Si la entrada es "resta", pide al usuario dos números y los resta
            "resta" => {
                println!("Introduce dos números separados por un espacio:");

                // Lee la entrada del usuario y la separa en dos números
                let numbers = input.split(" ").collect::<Vec<&str>>();

                // Convierte los números a enteros
                let num1 = numbers[0].parse::<i32>().expect("Error al convertir el primer número");
                let num2 = numbers[1].parse::<i32>().expect("Error al convertir el segundo número");

                // Resta los números y muestra el resultado
                let resultado = num1 - num2;
                println!("El resultado de la resta es: {}", resultado);
            }

            // Si la entrada es "multiplicación", pide al usuario dos números y los multiplica
            "multiplicación" => {
                println!("Introduce dos números separados por un espacio:");

                // Lee la entrada del usuario y la separa en dos números
                let numbers = input.split(" ").collect::<Vec<&str>>();

                // Convierte los números a enteros
                let num1 = numbers[0].parse::<i32>().expect("Error al convertir el primer número");
                let num2 = numbers[1].parse::<i32>().expect("Error al convertir el segundo número");

                // Multiplica los números y muestra el resultado
                let resultado = num1 * num2;
                println!("El resultado de la multiplicación es: {}", resultado);
            }

            // Si la entrada es "división", pide al usuario dos números y los divide
            "división" => {
                println!("Introduce dos números separados por un espacio:");

                // Lee la entrada del usuario y la separa en dos números
                let numbers = input.split(" ").collect::<Vec<&str>>();

                // Convierte los números a números flotantes
                let num1 = numbers[0].parse::<f32>().expect("Error al convertir el primer número");
                let num2 = numbers[1].parse::<f32>().expect("Error al convertir el segundo número");

                // Divide los números y muestra el resultado
                let resultado = num1 / num2;
                println!("El resultado de la división es: {}", resultado);
            }

            // Si la entrada no es ninguna de las opciones válidas, muestra un mensaje de error
            _ => {
                println!("Comando no reconocido. Introduce 'suma', 'resta', 'multiplicación', 'división' o 'salir'.");
            }
        }

        // Muestra una línea en blanco para separar las salidas
        println!();
    }
}
```

Este código es un programa interactivo que permite al usuario realizar operaciones aritméticas básicas (suma, resta, multiplicación y división) o salir del programa.

El programa usa un bucle infinito para seguir corriendo hasta que el usuario introduce la opción "salir".

Dentro del bucle infinito, el programa lee la entrada del usuario, la procesa y muestra el resultado.

La función `match` se utiliza para comprobar si la entrada del usuario es una de las opciones válidas.

Si la entrada no es ninguna de las opciones válidas, se muestra un mensaje de error.

El programa utiliza la clase `String` para almacenar la entrada del usuario y la función `trim()` para eliminar los espacios en blanco al final de la entrada.

También utiliza la función `split()` para separar la entrada en partes individuales y la función `parse()` para convertir las partes en números.

El programa utiliza la clase `Vec` para almacenar las partes de la entrada separadas y la función `collect()` para crear un vector a partir de una colección.

El programa utiliza la clase `io` para leer la entrada del usuario y mostrar los resultados de las operaciones aritméticas.