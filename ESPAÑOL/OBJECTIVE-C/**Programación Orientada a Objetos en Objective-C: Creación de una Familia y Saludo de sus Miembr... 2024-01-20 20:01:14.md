```objective-c
#import <Foundation/Foundation.h>

// Definición de la clase Persona
@interface Persona : NSObject {
    NSString *_nombre;
    int _edad;
}

// Propiedades de la clase Persona
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) int edad;

// Métodos de la clase Persona
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad;
- (void)saludar;

@end

// Implementación de la clase Persona
@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _edad = edad;
    }
    return self;
}

- (void)saludar {
    NSLog(@"Hola, mi nombre es %@ y tengo %d años.", _nombre, _edad);
}

@end

// Definición de la clase Familia
@interface Familia : NSObject {
    NSMutableArray *_miembros;
}

// Propiedades de la clase Familia
@property (nonatomic, strong) NSMutableArray *miembros;

// Métodos de la clase Familia
- (instancetype)init;
- (void)agregarMiembro:(Persona *)miembro;
- (void)saludar Familia;

@end

// Implementación de la clase Familia
@implementation Familia

- (instancetype)init {
    self = [super init];
    if (self) {
        _miembros = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)agregarMiembro:(Persona *)miembro {
    [_miembros addObject:miembro];
}

- (void)saludar Familia {
    for (Persona *miembro in _miembros) {
        [miembro saludar];
    }
}

@end

// Función principal del programa
int main(int argc, const char * argv[]) {
    // Crear una nueva familia
    Familia *familia = [[Familia alloc] init];
    
    // Agregar miembros a la familia
    Persona *padre = [[Persona alloc] initWithNombre:@"Juan" edad:40];
    Persona *madre = [[Persona alloc] initWithNombre:@"María" edad:38];
    Persona *hijo1 = [[Persona alloc] initWithNombre:@"Miguel" edad:10];
    Persona *hijo2 = [[Persona alloc] initWithNombre:@"Ana" edad:8];
    
    [familia agregarMiembro:padre];
    [familia agregarMiembro:madre];
    [familia agregarMiembro:hijo1];
    [familia agregarMiembro:hijo2];
    
    // Saludar a la familia
    [familia saludar Familia];
    
    return 0;
}
```

Explicación del código:

* La clase `Persona` representa a una persona con un nombre y una edad.
* La clase `Familia` representa a una familia con una lista de miembros.
* La función `main` crea una nueva familia y agrega miembros a ella.
* La función `saludar Familia` saluda a todos los miembros de la familia.

Este código es complejo porque utiliza múltiples clases, objetos y métodos para representar y manipular datos. También utiliza un bucle `for` para recorrer la lista de miembros de la familia y saludar a cada uno de ellos.