```objective-c
#import <Foundation/Foundation.h>

// Definir un protocolo para describir el comportamiento de una clase de servicio.
@protocol ServicioDeMensajes {

    // Método para enviar un mensaje.
    -(void) enviarMensaje:(NSString *)mensaje;

    // Método para recibir un mensaje.
    -(void) recibirMensaje:(NSString *)mensaje;

}

// Definir una implementación de la clase de servicio.
@interface ServicioDeMensajesImpl : NSObject <ServicioDeMensajes> {

    // Variable para almacenar la lista de mensajes recibidos.
    NSMutableArray *mensajesRecibidos;

}

// Método para enviar un mensaje.
-(void) enviarMensaje:(NSString *)mensaje {
    // Código para enviar el mensaje.
}

// Método para recibir un mensaje.
-(void) recibirMensaje:(NSString *)mensaje {

    // Agregar el mensaje a la lista de mensajes recibidos.
    [mensajesRecibidos addObject:mensaje];

}

@end

// Definir una clase para representar un cliente.
@interface Cliente {

    // Variable para almacenar el servicio de mensajes.
    id<ServicioDeMensajes> servicioDeMensajes;

}

// Método para inicializar el cliente.
-(id) initWithServicioDeMensajes:(id<ServicioDeMensajes>)servicio {

    // Guardar el servicio de mensajes.
    servicioDeMensajes = servicio;

    // Retornar la instancia del cliente.
    return self;

}

// Método para enviar un mensaje.
-(void) enviarMensaje:(NSString *)mensaje {

    // Llamar al método enviarMensaje del servicio de mensajes.
    [servicioDeMensajes enviarMensaje:mensaje];

}

// Método para recibir un mensaje.
-(void) recibirMensaje:(NSString *)mensaje {

    // Lógica para manejar el mensaje recibido.

}

@end

// Definir una clase para representar un servidor.
@interface Servidor {

    // Variable para almacenar la lista de clientes conectados.
    NSMutableArray *clientesConectados;

    // Variable para almacenar el servicio de mensajes.
    id<ServicioDeMensajes> servicioDeMensajes;

}

// Método para inicializar el servidor.
-(id) initWithServicioDeMensajes:(id<ServicioDeMensajes>)servicio {

    // Guardar el servicio de mensajes.
    servicioDeMensajes = servicio;

    // Crear la lista de clientes conectados.
    clientesConectados = [[NSMutableArray alloc] init];

    // Retornar la instancia del servidor.
    return self;

}

// Método para conectar un cliente al servidor.
-(void) conectarCliente:(Cliente *)cliente {

    // Agregar el cliente a la lista de clientes conectados.
    [clientesConectados addObject:cliente];

}

// Método para enviar un mensaje a todos los clientes conectados.
-(void) enviarMensajeATodos:(NSString *)mensaje {

    // Recorrer la lista de clientes conectados.
    for (Cliente *cliente in clientesConectados) {

        // Llamar al método enviarMensaje del cliente.
        [cliente enviarMensaje:mensaje];

    }

}

@end

// Definir una clase para representar el programa principal.
int main(int argc, char *argv[]) {

    // Crear una instancia del servicio de mensajes.
    ServicioDeMensajesImpl *servicioDeMensajes = [[ServicioDeMensajesImpl alloc] init];

    // Crear una instancia del servidor.
    Servidor *servidor = [[Servidor alloc] initWithServicioDeMensajes:servicioDeMensajes];

    // Crear una instancia del cliente.
    Cliente *cliente = [[Cliente alloc] initWithServicioDeMensajes:servicioDeMensajes];

    // Conectar el cliente al servidor.
    [servidor conectarCliente:cliente];

    // Enviar un mensaje desde el cliente al servidor.
    [cliente enviarMensaje:@"Hola, servidor!"];

    // Enviar un mensaje desde el servidor a todos los clientes conectados.
    [servidor enviarMensajeATodos:@"Hola, clientes!"];

    // Liberar los recursos utilizados.

    return 0;

}
```

Explicación del código:

* El código define un protocolo `ServicioDeMensajes` que describe el comportamiento de una clase de servicio de mensajes.
* La clase `ServicioDeMensajesImpl` es una implementación del protocolo `ServicioDeMensajes`. Proporciona métodos para enviar y recibir mensajes.
* La clase `Cliente` representa un cliente que puede enviar y recibir mensajes a través del servicio de mensajes.
* La clase `Servidor` representa un servidor que puede conectarse a múltiples clientes y enviarles mensajes.
* El método `main()` crea instancias del servicio de mensajes, el servidor y el cliente, conecta el cliente al servidor y envía mensajes entre ellos.