```objective-c
#import <Foundation/Foundation.h>

// Protocolo que define los métodos que debe implementar una clase que desee ser un delegado de una instancia de la clase `MiClase`.
@protocol MiProtocoloDelegado <NSObject>

- (void)miMetodoDelegado:(NSString *)parametro;

@end

// Clase que implementa el protocolo `MiProtocoloDelegado`.
@interface MiDelegado : NSObject <MiProtocoloDelegado>

- (void)miMetodoDelegado:(NSString *)parametro;

@end

// Clase principal.
@interface MiClase : NSObject

// Propiedad que almacena el delegado de la instancia.
@property (nonatomic, weak) id<MiProtocoloDelegado> delegado;

// Método que llama al método `miMetodoDelegado` del delegado.
- (void)llamarMetodoDelegado:(NSString *)parametro;

@end

// Implementación de los métodos de la clase `MiClase`.
@implementation MiClase

- (void)llamarMetodoDelegado:(NSString *)parametro {
    if (self.delegado && [self.delegado respondsToSelector:@selector(miMetodoDelegado:)]) {
        [self.delegado miMetodoDelegado:parametro];
    }
}

@end

// Implementación de los métodos de la clase `MiDelegado`.
@implementation MiDelegado

- (void)miMetodoDelegado:(NSString *)parametro {
    NSLog(@"Método delegado llamado con el parámetro: %@", parametro);
}

@end

// Función principal del programa.
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crea una instancia de la clase `MiClase`.
        MiClase *miClase = [[MiClase alloc] init];

        // Crea una instancia de la clase `MiDelegado`.
        MiDelegado *miDelegado = [[MiDelegado alloc] init];

        // Establece el delegado de la instancia de `MiClase`.
        miClase.delegado = miDelegado;

        // Llama al método `llamarMetodoDelegado` de la instancia de `MiClase`, pasando un parámetro.
        [miClase llamarMetodoDelegado:@"Hola, mundo!"];

        // Sal de la aplicación.
        return 0;
    }
}
```

Explicación del código:

* El protocolo `MiProtocoloDelegado` define un método llamado `miMetodoDelegado` que toma un parámetro de tipo `NSString`. Las clases que deseen ser delegadas de una instancia de la clase `MiClase` deben implementar este protocolo.
* La clase `MiDelegado` implementa el protocolo `MiProtocoloDelegado` y proporciona una implementación del método `miMetodoDelegado`. Este método simplemente imprime el parámetro que se le pasa en la consola.
* La clase `MiClase` tiene una propiedad llamada `delegado` que almacena el delegado de la instancia. También tiene un método llamado `llamarMetodoDelegado` que llama al método `miMetodoDelegado` del delegado, si existe.
* En la función `main` del programa, se crea una instancia de la clase `MiClase` y una instancia de la clase `MiDelegado`. El delegado de la instancia de `MiClase` se establece en la instancia de `MiDelegado`. Luego, se llama al método `llamarMetodoDelegado` de la instancia de `MiClase`, pasando un parámetro. Esto hace que se llame al método `miMetodoDelegado` de la instancia de `MiDelegado`, que imprime el parámetro en la consola.