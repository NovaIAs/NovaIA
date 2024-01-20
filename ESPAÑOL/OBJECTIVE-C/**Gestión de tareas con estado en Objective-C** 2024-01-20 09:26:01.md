```objective-c
// Importación de frameworks y bibliotecas necesarias

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

// Definición de constantes y tipos de datos personalizados

typedef enum {
    kEstadoInicial,
    kEstadoEjecutando,
    kEstadoPausado,
    kEstadoFinalizado
} EstadoTarea;

// Declaración de la clase Tarea

@interface Tarea : NSObject {
    NSString *_nombre;
    EstadoTarea _estado;
    NSTimer *_temporizador;
}

// Propiedades de la clase Tarea

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) EstadoTarea estado;
@property (nonatomic, strong) NSTimer *temporizador;

// Métodos de la clase Tarea

- (instancetype)initWithNombre:(NSString *)nombre;
- (void)iniciar;
- (void)pausar;
- (void)finalizar;

@end

// Implementación de la clase Tarea

@implementation Tarea

// Método inicializador

- (instancetype)initWithNombre:(NSString *)nombre {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _estado = kEstadoInicial;
        _temporizador = nil;
    }
    return self;
}

// Método para iniciar la tarea

- (void)iniciar {
    if (_estado == kEstadoInicial) {
        _estado = kEstadoEjecutando;
        _temporizador = [NSTimer scheduledTimerWithTimeInterval:1.0 target:self selector:@selector(ejecutarTarea) userInfo:nil repeats:YES];
    }
}

// Método para pausar la tarea

- (void)pausar {
    if (_estado == kEstadoEjecutando) {
        _estado = kEstadoPausado;
        [_temporizador invalidate];
        _temporizador = nil;
    }
}

// Método para finalizar la tarea

- (void)finalizar {
    if (_estado != kEstadoFinalizado) {
        _estado = kEstadoFinalizado;
        [_temporizador invalidate];
        _temporizador = nil;
    }
}

// Método para ejecutar la tarea

- (void)ejecutarTarea {
    NSLog(@"Ejecutando tarea: %@", _nombre);
}

@end

// Clase principal

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creación de una tarea

        Tarea *tarea1 = [[Tarea alloc] initWithNombre:@"Tarea 1"];

        // Inicio de la tarea

        [tarea1 iniciar];

        // Pausa de la tarea después de 5 segundos

        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(5 * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
            [tarea1 pausar];
        });

        // Finalización de la tarea después de 10 segundos

        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(10 * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
            [tarea1 finalizar];
        });

        // Ejecución del bucle principal de la aplicación

        [[NSRunLoop mainRunLoop] run];
    }
    return 0;
}
```

Explicación del código:

1. **Importación de frameworks y bibliotecas necesarias**: Se importan los frameworks y bibliotecas necesarios para el funcionamiento del programa, como Foundation y AppKit.

2. **Definición de constantes y tipos de datos personalizados**: Se define una enumeración llamada EstadoTarea para representar los diferentes estados en los que puede encontrarse una tarea (inicial, ejecutando, pausado, finalizado).

3. **Declaración de la clase Tarea**: Se declara la clase Tarea, que representa una tarea que se puede iniciar, pausar y finalizar.

4. **Propiedades de la clase Tarea**: Se declaran las propiedades de la clase Tarea, que incluyen el nombre de la tarea, su estado actual y un temporizador para controlar la ejecución de la tarea.

5. **Métodos de la clase Tarea**: Se declaran los métodos de la clase Tarea, que incluyen un método inicializador, métodos para iniciar, pausar y finalizar la tarea, y un método para ejecutar la tarea.

6. **Implementación de la clase Tarea**: Se implementa la clase Tarea, definiendo el comportamiento de los métodos declarados anteriormente.

7. **Clase principal**: Se define la clase principal del programa, que contiene el método main().

8. **Creación de una tarea**: Se crea una instancia de la clase Tarea con el nombre "Tarea 1".

9. **Inicio de la tarea**: Se llama al método iniciar() de la tarea para iniciarla.

10. **Pausa de la tarea después de 5 segundos**: Se utiliza la función dispatch_after() para pausar la tarea después de 5 segundos.

11. **Finalización de la tarea después de 10 segundos**: Se utiliza la función dispatch_after() para finalizar la tarea después de 10 segundos.

12. **Ejecución del bucle principal de la aplicación**: Se llama al método run() del bucle principal de la aplicación para ejecutar el programa.

Este código crea una tarea que se ejecuta durante 5 segundos, se pausa durante 5 segundos y se finaliza después de 10 segundos. La tarea se ejecuta utilizando un temporizador NSTimer, que se crea cuando se inicia la tarea y se invalida cuando se pausa o finaliza la tarea.