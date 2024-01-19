```rust
// Librerías que importa el código.
use std::io;
use std::collections::HashMap;
use std::collections::HashSet;

// Función principal.
fn main() {

    // Creamos un mapa vacío para almacenar los nombres de los productos y sus precios.
    let mut productos: HashMap<String, f32> = HashMap::new();

    // Creamos un conjunto vacío para almacenar los nombres de los productos que ya se han vendido.
    let mut vendidos: HashSet<String> = HashSet::new();

    // Bucle principal del programa.
    loop {

        // Mostramos un mensaje al usuario pidiéndole que introduzca una opción.
        println!("¿Qué desea hacer?");
        println("1. Añadir un producto.");
        println("2. Vender un producto.");
        println("3. Mostrar el inventario.");
        println("4. Salir.");

        // Leemos la opción introducida por el usuario.
        let mut opcion = String::new();
        io::stdin().read_line(&mut opcion).expect("Error al leer la opción.");

        // Quitamos los espacios en blanco de la opción.
        let opcion = opcion.trim();

        // Procesamos la opción introducida por el usuario.
        match opcion {

            // Si el usuario elige añadir un producto, pedimos el nombre del producto y su precio, y lo añadimos al mapa.
            "1" => {

                // Pedimos el nombre del producto.
                println!("Introduzca el nombre del producto:");
                let mut nombre = String::new();
                io::stdin().read_line(&mut nombre).expect("Error al leer el nombre.");

                // Quitamos los espacios en blanco del nombre.
                let nombre = nombre.trim();

                // Pedimos el precio del producto.
                println!("Introduzca el precio del producto:");
                let mut precio = String::new();
                io::stdin().read_line(&mut precio).expect("Error al leer el precio.");

                // Quitamos los espacios en blanco del precio.
                let precio = precio.trim();

                // Convertimos el precio a un número flotante.
                let precio = precio.parse::<f32>().expect("Error al convertir el precio a un número.");

                // Añadimos el producto al mapa.
                productos.insert(nombre, precio);

                // Mostramos un mensaje al usuario indicando que el producto se ha añadido correctamente.
                println!("El producto se ha añadido correctamente.");

            }

            // Si el usuario elige vender un producto, pedimos el nombre del producto y lo marcamos como vendido.
            "2" => {

                // Pedimos el nombre del producto.
                println!("Introduzca el nombre del producto:");
                let mut nombre = String::new();
                io::stdin().read_line(&mut nombre).expect("Error al leer el nombre.");

                // Quitamos los espacios en blanco del nombre.
                let nombre = nombre.trim();

                // Comprobamos si el producto existe en el inventario.
                if productos.contains_key(&nombre) {

                    // Marcamos el producto como vendido.
                    vendidos.insert(nombre);

                    // Mostramos un mensaje al usuario indicando que el producto se ha vendido correctamente.
                    println!("El producto se ha vendido correctamente.");

                } else {

                    // Mostramos un mensaje al usuario indicando que el producto no existe en el inventario.
                    println!("El producto no existe en el inventario.");

                }

            }

            // Si el usuario elige mostrar el inventario, mostramos el nombre y el precio de todos los productos del inventario.
            "3" => {

                // Mostramos un mensaje al usuario indicando que se va a mostrar el inventario.
                println!("Inventario:");

                // Recorremos el mapa de productos.
                for (nombre, precio) in &productos {

                    // Comprobamos si el producto se ha vendido.
                    if !vendidos.contains(nombre) {

                        // Mostramos el nombre y el precio del producto.
                        println!("{}: ${}", nombre, precio);

                    }

                }

            }

            // Si el usuario elige salir, salimos del programa.
            "4" => {

                // Salimos del programa.
                break;

            }

            // Si el usuario introduce una opción incorrecta, mostramos un mensaje de error.
            _ => {

                // Mostramos un mensaje de error.
                println!("Opción incorrecta.");

            }

        }

    }

}
```

Este código es un programa de gestión de inventario en Rust. El programa permite al usuario añadir productos al inventario, vender productos del inventario y mostrar el inventario. El programa utiliza un mapa para almacenar los productos y sus precios, y un conjunto para almacenar los nombres de los productos que ya se han vendido.

El programa comienza creando un mapa vacío para almacenar los productos y sus precios, y un conjunto vacío para almacenar los nombres de los productos que ya se han vendido.

A continuación, el programa entra en un bucle principal, donde el usuario puede elegir una de las siguientes opciones:

* **Añadir un producto:** El usuario puede añadir un producto al inventario introduciendo el nombre del producto y su precio.
* **Vender un producto:** El usuario puede vender un producto del inventario introduciendo el nombre del producto.
* **Mostrar el inventario:** El usuario puede mostrar el inventario mostrando el nombre y el precio de todos los productos del inventario que no se han vendido.
* **Salir:** El usuario puede salir del programa.

El usuario elige una opción introduciendo un número del 1 al 4. El programa procesa la opción introducida por el usuario y realiza la acción correspondiente.

El programa sale del bucle principal cuando el usuario elige la opción de salir.