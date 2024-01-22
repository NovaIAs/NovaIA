```rust
use std::io::{self, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::mpsc;
use std::thread;

// Función principal del servidor
fn main() -> io::Result<()> {
    // Creamos una dirección IP y un puerto para el servidor
    let address = "127.0.0.1:8080";

    // Creamos un socket TCP
    let listener = TcpListener::bind(address)?;

    // Creamos un canal para comunicarnos con los hilos de los clientes
    let (tx, rx) = mpsc::channel();

    // Creamos un hilo para escuchar a los nuevos clientes
    thread::spawn(move || {
        loop {
            // Aceptamos una nueva conexión
            let (stream, _) = listener.accept().unwrap();

            // Enviamos el stream al canal
            tx.send(stream).unwrap();
        }
    });

    // Creamos un ciclo para procesar los mensajes de los clientes
    loop {
        // Recibimos un mensaje del canal
        let stream = rx.recv().unwrap();

        // Creamos un hilo para procesar el mensaje del cliente
        thread::spawn(move || {
            // Leemos el mensaje del cliente
            let mut buffer = [0u8; 1024];
            let bytes_read = stream.read(&mut buffer).unwrap();

            // Convertimos el mensaje en una cadena
            let request = String::from_utf8_lossy(&buffer[..bytes_read]);

            // Imprimimos el mensaje del cliente
            println!("{}", request);

            // Escribimos una respuesta al cliente
            stream.write_all(b"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello World!").unwrap();

            // Cerramos el socket
            stream.shutdown(std::net::Shutdown::Both).unwrap();
        });
    }

    Ok(())
}
```

**Explicación del código:**

1. **Importamos las librerías necesarias:**

   - `std::io`: Para manejar la entrada y salida.
   - `std::net`: Para manejar las conexiones de red.
   - `std::sync::mpsc`: Para crear un canal de comunicación entre hilos.
   - `std::thread`: Para crear hilos.

2. **Creamos una dirección IP y un puerto para el servidor:**

   - `let address = "127.0.0.1:8080";`

   Esta línea crea una dirección IP y un puerto para el servidor. La dirección IP es `127.0.0.1`, que es la dirección IP de la máquina local. El puerto es `8080`.

3. **Creamos un socket TCP:**

   - `let listener = TcpListener::bind(address)?;`

   Esta línea crea un socket TCP y lo enlaza a la dirección IP y al puerto especificados. El socket TCP es un tipo de socket que se utiliza para establecer conexiones entre dos aplicaciones en diferentes máquinas.

4. **Creamos un canal para comunicarnos con los hilos de los clientes:**

   - `let (tx, rx) = mpsc::channel();`

   Esta línea crea un canal de comunicación entre hilos. El canal es un tipo de cola que se utiliza para enviar y recibir mensajes entre hilos.

5. **Creamos un hilo para escuchar a los nuevos clientes:**

   - `thread::spawn(move || { ... });`

   Esta línea crea un nuevo hilo que se encarga de escuchar a los nuevos clientes. El hilo se ejecuta de forma concurrente con el hilo principal.

6. **Creamos un ciclo para procesar los mensajes de los clientes:**

   - `loop { ... }`

   Esta línea crea un ciclo infinito que se encarga de procesar los mensajes de los clientes.

7. **Recibimos un mensaje del canal:**

   - `let stream = rx.recv().unwrap();`

   Esta línea recibe un mensaje del canal. El mensaje es un stream TCP que representa una conexión con un cliente.

8. **Creamos un hilo para procesar el mensaje del cliente:**

   - `thread::spawn(move || { ... });`

   Esta línea crea un nuevo hilo que se encarga de procesar el mensaje del cliente. El hilo se ejecuta de forma concurrente con el hilo principal y con el hilo que escucha a los nuevos clientes.

9. **Leemos el mensaje del cliente:**

   - `let mut buffer = [0u8; 1024];`
   - `let bytes_read = stream.read(&mut buffer).unwrap();`

   Estas líneas leen el mensaje del cliente. El mensaje se almacena en el búfer `buffer`. La variable `bytes_read` contiene el número de bytes leídos del cliente.

10. **Convertimos el mensaje en una cadena:**

    - `let request = String::from_utf8_lossy(&buffer[..bytes_read]);`

    Esta línea convierte el mensaje en una cadena. La función `String::from_utf8_lossy()` convierte una secuencia de bytes en una cadena.

11. **Imprimimos el mensaje del cliente:**

    - `println!("{}", request);`

    Esta línea imprime el mensaje del cliente en la consola.

12. **Escribimos una respuesta al cliente:**

    - `stream.write_all(b"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello World!").unwrap();`

    Esta línea escribe una respuesta al cliente. La respuesta es una cadena de bytes que contiene el encabezado HTTP y el cuerpo de la respuesta.

13. **Cerramos el socket:**

    - `stream.shutdown(std::net::Shutdown::Both).unwrap();`

    Esta línea cierra el socket. El socket se cierra para evitar que el cliente envíe más mensajes.