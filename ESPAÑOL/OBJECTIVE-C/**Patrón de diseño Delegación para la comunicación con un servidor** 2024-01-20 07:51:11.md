```objective-c
#import <Foundation/Foundation.h>

// Definir una interfaz para un protocolo de comunicación.
@protocol ComunicacionProtocolo
- (void)enviarMensaje:(NSString *)mensaje;
- (NSString *)recibirMensaje;
@end

// Implementar el protocolo de comunicación en una clase.
@interface Comunicacion : NSObject <ComunicacionProtocolo>
- (void)enviarMensaje:(NSString *)mensaje;
- (NSString *)recibirMensaje;
@end

// Implementar los métodos del protocolo de comunicación en la clase.
@implementation Comunicacion
- (void)enviarMensaje:(NSString *)mensaje {
  // Código para enviar el mensaje.
}

- (NSString *)recibirMensaje {
  // Código para recibir el mensaje.
}
@end

// Definir una interfaz para una clase que gestiona la comunicación.
@interface GestorComunicacion : NSObject
- (void)conectarConServidor;
- (void)desconectarDelServidor;
@end

// Implementar la clase que gestiona la comunicación.
@implementation GestorComunicacion
- (void)conectarConServidor {
  // Código para conectar con el servidor.
}

- (void)desconectarDelServidor {
  // Código para desconectar del servidor.
}
@end

// Definir una clase que utiliza el protocolo de comunicación.
@interface Cliente : NSObject
- (void)enviarMensaje:(NSString *)mensaje;
- (NSString *)recibirMensaje;
@end

// Implementar la clase que utiliza el protocolo de comunicación.
@implementation Cliente
- (void)enviarMensaje:(NSString *)mensaje {
  // Crear una instancia de la clase que implementa el protocolo de comunicación.
  Comunicacion *comunicacion = [[Comunicacion alloc] init];

  // Utilizar los métodos del protocolo de comunicación para enviar el mensaje.
  [comunicacion enviarMensaje:mensaje];
}

- (NSString *)recibirMensaje {
  // Crear una instancia de la clase que implementa el protocolo de comunicación.
  Comunicacion *comunicacion = [[Comunicacion alloc] init];

  // Utilizar los métodos del protocolo de comunicación para recibir el mensaje.
  return [comunicacion recibirMensaje];
}
@end

// Definir una clase que utiliza la clase que gestiona la comunicación.
@interface Aplicacion : NSObject
- (void)iniciarComunicacion;
- (void)finalizarComunicacion;
@end

// Implementar la clase que utiliza la clase que gestiona la comunicación.
@implementation Aplicacion
- (void)iniciarComunicacion {
  // Crear una instancia de la clase que gestiona la comunicación.
  GestorComunicacion *gestorComunicacion = [[GestorComunicacion alloc] init];

  // Utilizar los métodos de la clase que gestiona la comunicación para conectar con el servidor.
  [gestorComunicacion conectarConServidor];
}

- (void)finalizarComunicacion {
  // Crear una instancia de la clase que gestiona la comunicación.
  GestorComunicacion *gestorComunicacion = [[GestorComunicacion alloc] init];

  // Utilizar los métodos de la clase que gestiona la comunicación para desconectar del servidor.
  [gestorComunicacion desconectarDelServidor];
}
@end

// Definir la función principal del programa.
int main(int argc, const char * argv[]) {
  @autoreleasepool {
    // Crear una instancia de la clase que utiliza la clase que gestiona la comunicación.
    Aplicacion *aplicacion = [[Aplicacion alloc] init];

    // Utilizar los métodos de la clase que utiliza la clase que gestiona la comunicación para iniciar la comunicación.
    [aplicacion iniciarComunicacion];

    // Crear una instancia de la clase que utiliza el protocolo de comunicación.
    Cliente *cliente = [[Cliente alloc] init];

    // Utilizar los métodos de la clase que utiliza el protocolo de comunicación para enviar un mensaje.
    [cliente enviarMensaje:@"Hola, mundo!"];

    // Utilizar los métodos de la clase que utiliza el protocolo de comunicación para recibir un mensaje.
    NSString *mensaje = [cliente recibirMensaje];

    // Mostrar el mensaje recibido en la consola.
    NSLog(@"%@", mensaje);

    // Utilizar los métodos de la clase que utiliza la clase que gestiona la comunicación para finalizar la comunicación.
    [aplicacion finalizarComunicacion];
  }

  return 0;
}
```

Explicación:

* El código anterior es un programa sencillo que utiliza el patrón de diseño **Delegación** para gestionar la comunicación con un servidor.
* El programa está dividido en varias clases:
    * CommunicacionProtocolo: Define el protocolo de comunicación que se utilizará para enviar y recibir mensajes.
    * Comunicacion: Implementa el protocolo de comunicación y proporciona los métodos para enviar y recibir mensajes.
    * GestorComunicacion: Gestiona la conexión y desconexión del servidor.
    * Cliente: Utiliza el protocolo de comunicación para enviar y recibir mensajes.
    * Aplicacion: Inicia y finaliza la comunicación con el servidor.
* El programa comienza en la función main(), que crea una instancia de la clase Aplicacion y llama a sus métodos para iniciar la comunicación con el servidor.
* A continuación, se crea una instancia de la clase Cliente y se utilizan sus métodos para enviar y recibir mensajes.
* El programa finaliza cuando se llama al método finalizarComunicacion() de la clase Aplicacion, lo que desconecta al cliente del servidor.