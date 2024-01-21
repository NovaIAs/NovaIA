```
// El código crea un servidor web simple que recibe solicitudes HTTP en el puerto 8080.
// El servidor devuelve un mensaje de bienvenida al cliente.

use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};

fn main() {
    // Creamos un escuchador que se une a la dirección IP local en el puerto 8080.
    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();

    // El ciclo infinito espera a que lleguen las conexiones de los clientes.
    loop {
        // Aceptamos la conexión del cliente y la almacenamos en la variable `stream`.
        let (stream, _) = listener.accept().unwrap();

        // Creamos una nueva tarea para manejar la conexión del cliente.
        std::thread::spawn(move || {
            // Leemos los datos del cliente y los almacenamos en la variable `buffer`.
            let mut buffer = [0; 1024];
            stream.read(&mut buffer).unwrap();

            // Convertimos el mensaje del cliente de bytes a una cadena UTF-8.
            let message = String::from_utf8(buffer).unwrap();

            // Imprimimos el mensaje del cliente en la consola.
            println!("Mensaje del cliente: {}", message);

            // Creamos una respuesta al cliente.
            let response = "HTTP/1.1 200 OK\nContent-Type: text/plain\n\nBienvenido al servidor web de Rust!\n";

            // Enviamos la respuesta al cliente.
            stream.write(response.as_bytes()).unwrap();

            // Cerramos la conexión con el cliente.
            stream.flush().unwrap();
        });
    }
}
```

Explicación del código:

* La función `main()` es el punto de entrada del programa.
* La variable `listener` es un escuchador que se une a la dirección IP local en el puerto 8080.
* El ciclo `loop` espera a que lleguen las conexiones de los clientes.
* La función `accept()` acepta una conexión del cliente y devuelve una tupla con la conexión y la dirección del cliente.
* La tarea `std::thread::spawn()` crea una nueva tarea para manejar la conexión del cliente.
* La función `read()` lee los datos del cliente y los almacena en la variable `buffer`.
* La función `from_utf8()` convierte el mensaje del cliente de bytes a una cadena UTF-8.
* La función `println()` imprime el mensaje del cliente en la consola.
* La variable `response` es una respuesta al cliente.
* La función `write()` envía la respuesta al cliente.
* La función `flush()` vacía el búfer de escritura de la conexión.
* La función `unwrap()` se utiliza para manejar los errores.