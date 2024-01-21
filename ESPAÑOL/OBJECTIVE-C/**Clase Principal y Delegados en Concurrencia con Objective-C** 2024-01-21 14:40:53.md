```objective-c
// Importar las bibliotecas necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Definir la interfaz de la clase principal
@interface MiClasePrincipal : NSObject

// Propiedades
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSArray *lista;

// Métodos
- (void)saludar;
- (void)imprimirLista;

@end

// Implementar la clase principal
@implementation MiClasePrincipal

// Método para saludar
- (void)saludar {
    NSLog(@"¡Hola, %@!", self.nombre);
}

// Método para imprimir la lista
- (void)imprimirLista {
    for (NSString *elemento in self.lista) {
        NSLog(@"%@", elemento);
    }
}

@end

// Definir la interfaz del delegado
@protocol MiProtocoloDelegado

// Método que se llamará cuando se complete una tarea
- (void)tareaCompletada:(NSString *)resultado;

@end

// Implementar la clase delegada
@interface MiClaseDelegada : NSObject <MiProtocoloDelegado>

// Método para manejar la finalización de la tarea
- (void)tareaCompletada:(NSString *)resultado {
    NSLog(@"La tarea se completó con el resultado: %@", resultado);
}

@end

// Definir el bloque de código que se ejecutará en un hilo secundario
void tareaEnSegundoPlano(MiProtocoloDelegado *delegado) {
    // Simular un retraso de 2 segundos
    sleep(2);
    
    // Llamar al método del delegado para indicar que la tarea se completó
    [delegado tareaCompletada:@"¡Tarea completada!"];
}

// Crear una instancia de la clase principal
MiClasePrincipal *clasePrincipal = [[MiClasePrincipal alloc] init];

// Asignar valores a las propiedades
clasePrincipal.nombre = @"Juan";
clasePrincipal.lista = @[@"Manzana", @"Naranja", @"Plátano"];

// Saludar
[clasePrincipal saludar];

// Imprimir la lista
[clasePrincipal imprimirLista];

// Crear una instancia de la clase delegada
MiClaseDelegada *claseDelegada = [[MiClaseDelegada alloc] init];

// Crear una cola de tareas en segundo plano
dispatch_queue_t colaSecundaria = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);

// Enviar la tarea a la cola de tareas en segundo plano
dispatch_async(colaSecundaria, ^{
    tareaEnSegundoPlano(claseDelegada);
});
```

Explicación del código:

* La clase `MiClasePrincipal` tiene dos propiedades: `nombre` y `lista`. También tiene dos métodos: `saludar` e `imprimirLista`. El método `saludar` simplemente imprime un mensaje en la consola. El método `imprimirLista` imprime cada elemento de la lista en la consola.
* La clase `MiClaseDelegada` implementa el protocolo `MiProtocoloDelegado`. El protocolo `MiProtocoloDelegado` tiene un solo método: `tareaCompletada`. El método `tareaCompletada` se llamará cuando se complete una tarea.
* La función `tareaEnSegundoPlano` se ejecutará en un hilo secundario. La función `tareaEnSegundoPlano` simula un retraso de 2 segundos y luego llama al método `tareaCompletada` del delegado para indicar que la tarea se completó.
* En el método `main` del programa, se crea una instancia de la clase `MiClasePrincipal` y se asignan valores a sus propiedades. A continuación, se llama a los métodos `saludar` e `imprimirLista` de la clase `MiClasePrincipal`.
* Se crea una instancia de la clase `MiClaseDelegada` y se envía la tarea a la cola de tareas en segundo plano. La cola de tareas en segundo plano ejecutará la tarea en un hilo secundario.
* Cuando la tarea se completa, se llama al método `tareaCompletada` de la clase `MiClaseDelegada`. El método `tareaCompletada` imprime un mensaje en la consola.