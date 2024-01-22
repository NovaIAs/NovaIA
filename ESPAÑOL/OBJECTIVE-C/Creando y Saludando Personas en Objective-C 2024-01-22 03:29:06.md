```objective-c
// Importamos las bibliotecas necesarias.
#import <Foundation/Foundation.h>

// Creamos una clase llamada "Persona".
@interface Persona : NSObject

// Creamos las propiedades de la clase "Persona".
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSString *apellido;
@property (nonatomic, assign) NSInteger edad;

// Creamos el método inicializador de la clase "Persona".
- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad;

// Creamos el método "saludar" de la clase "Persona".
- (void)saludar;

@end

// Implementamos la clase "Persona".
@implementation Persona

// Implementamos el método inicializador de la clase "Persona".
- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.apellido = apellido;
        self.edad = edad;
    }
    return self;
}

// Implementamos el método "saludar" de la clase "Persona".
- (void)saludar {
    NSLog(@"¡Hola, mi nombre es %@ %@ y tengo %ld años!", self.nombre, self.apellido, self.edad);
}

@end

// Creamos una clase llamada "Main".
@interface Main : NSObject

@end

// Implementamos la clase "Main".
@implementation Main

// Creamos el método "main" de la clase "Main".
+ (void)main {
    // Creamos un objeto de la clase "Persona".
    Persona *persona = [[Persona alloc] initWithNombre:@"Juan" apellido:@"Pérez" edad:25];

    // Llamamos al método "saludar" del objeto "persona".
    [persona saludar];
}

@end

// Llamamos al método "main" de la clase "Main".
[Main main];
```

Este código crea una clase llamada "Persona" con tres propiedades: "nombre", "apellido" y "edad". También crea un método inicializador para la clase "Persona" y un método llamado "saludar" que imprime el nombre, el apellido y la edad de la persona.

Luego, crea una clase llamada "Main" y un método llamado "main" que crea un objeto de la clase "Persona" y llama al método "saludar" del objeto.

Finalmente, llama al método "main" de la clase "Main" para ejecutar el programa.