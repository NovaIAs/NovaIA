```objective-c
#import <Foundation/Foundation.h>

// Definir clase
@interface Delegado : NSObject

// Definir protocolo
@protocol Protocolo

// Declarar métodos del protocolo
- (void)metodo1;
- (void)metodo2:(NSString *)argumento;

// Fin del protocolo
@end

// Fin de definición de clase

// Implementar clase
@implementation Delegado

// Implementar métodos del protocolo
- (void)metodo1 {
    NSLog(@"Se ha llamado al método 1");
}

- (void)metodo2:(NSString *)argumento {
    NSLog(@"Se ha llamado al método 2 con el argumento: %@", argumento);
}

// Fin de implementación de clase

// Crear instancia de la clase Delegado
Delegado *delegado = [[Delegado alloc] init];

// Comprobar si el delegado implementa el protocolo Protocolo
if ([delegado conformsToProtocol:@protocol(Protocolo)]) {
    // Si el delegado implementa el protocolo, llamar a sus métodos
    [delegado metodo1];
    [delegado metodo2:@"¡Hola mundo!"];
} else {
    // Si el delegado no implementa el protocolo, mostrar un error
    NSLog(@"El delegado no implementa el protocolo Protocolo");
}
```

Explicación del código:

1. Definimos una clase `Delegado` que implementa el protocolo `Protocolo`.
2. En el protocolo `Protocolo` declaramos dos métodos: `metodo1` y `metodo2`.
3. En la clase `Delegado` implementamos los métodos del protocolo.
4. Creamos una instancia de la clase `Delegado`.
5. Comprobamos si el delegado implementa el protocolo `Protocolo`.
6. Si el delegado implementa el protocolo, llamamos a sus métodos.
7. Si el delegado no implementa el protocolo, mostramos un error.

Este código demuestra cómo implementar un protocolo en Objective-C. Los protocolos se utilizan para definir una interfaz que puede ser implementada por diferentes clases. Esto permite desacoplar el código y hacerlo más flexible y reutilizable.