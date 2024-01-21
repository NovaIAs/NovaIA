```objective-c
// Incluye las librerías necesarias
#import <Foundation/Foundation.h>

// Define la clase principal
@interface MiClase : NSObject

// Define las propiedades de la clase
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSArray *lista;
@property (nonatomic, assign) NSInteger edad;

// Define los métodos de la clase
- (id)initWithNombre:(NSString *)nombre lista:(NSArray *)lista edad:(NSInteger)edad;
- (void)imprimirInformacion;

@end

// Implementa la clase
@implementation MiClase

// Define el método inicializador
- (id)initWithNombre:(NSString *)nombre lista:(NSArray *)lista edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _lista = lista;
        _edad = edad;
    }
    return self;
}

// Define el método para imprimir información
- (void)imprimirInformacion {
    NSLog(@"Nombre: %@", _nombre);
    NSLog(@"Lista: %@", _lista);
    NSLog(@"Edad: %ld", _edad);
}

@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crea una instancia de la clase MiClase
        MiClase *miClase = [[MiClase alloc] initWithNombre:@"Mi Nombre" lista:@[@"Uno", @"Dos", @"Tres"] edad:20];

        // Imprime la información de la instancia
        [miClase imprimirInformacion];
    }
    return 0;
}
```

Explicación del código:

* La primera línea incluye la librería `Foundation`, que es necesaria para usar objetos y funcionalidades básicas de Objective-C.
* La segunda línea define la clase principal `MiClase`, que es una clase NSObject, la clase base de todas las clases en Objective-C.
* Las siguientes tres líneas definen las propiedades de la clase `MiClase`: `nombre`, `lista` y `edad`. Las propiedades son variables que pertenecen a la instancia de una clase.
* Las siguientes cinco líneas definen los métodos de la clase `MiClase`: `initWithNombre:lista:edad:`, que es el método inicializador, y `imprimirInformacion`, que es un método que imprime la información de la instancia.
* La función `main` es la función principal del programa. Es donde se crea una instancia de la clase `MiClase` y se llama al método `imprimirInformacion` para imprimir la información de la instancia.