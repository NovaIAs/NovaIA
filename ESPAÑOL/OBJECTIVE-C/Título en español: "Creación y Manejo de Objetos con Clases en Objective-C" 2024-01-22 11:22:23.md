```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSString *apellidos;
@property (nonatomic, assign) NSInteger edad;

- (instancetype)initWithNombre:(NSString *)nombre apellidos:(NSString *)apellidos edad:(NSInteger)edad;
- (NSString *)nombreCompleto;

@end

@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre apellidos:(NSString *)apellidos edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _apellidos = apellidos;
        _edad = edad;
    }
    return self;
}

- (NSString *)nombreCompleto {
    return [NSString stringWithFormat:@"%@ %@", _nombre, _apellidos];
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear un objeto de la clase Persona
        Persona *persona = [[Persona alloc] initWithNombre:@"Juan" apellidos:@"García" edad:25];

        // Imprimir el nombre completo de la persona
        NSLog(@"Nombre completo: %@", persona.nombreCompleto);

        // Crear un array de objetos de la clase Persona
        NSArray *personas = @[
            [[Persona alloc] initWithNombre:@"María" apellidos:@"López" edad:28],
            [[Persona alloc] initWithNombre:@"Pedro" apellidos:"Sánchez" edad:32],
            [[Persona alloc] initWithNombre:"Ana" apellidos:"Fernández" edad:22]
        ];

        // Iterar sobre el array de personas
        for (Persona *persona in personas) {
            // Imprimir el nombre completo y la edad de la persona
            NSLog(@"Nombre completo: %@, Edad: %ld", persona.nombreCompleto, persona.edad);
        }
    }
    return 0;
}
```

Explicación del código:

* La clase `Persona` representa un objeto persona con las propiedades `nombre`, `apellidos` y `edad`.
* La función `main()` es el punto de entrada del programa.
* Se crea un objeto de la clase `Persona` con los datos de una persona específica.
* Se imprime el nombre completo de la persona.
* Se crea un array de objetos de la clase `Persona` con los datos de varias personas.
* Se itera sobre el array de personas y se imprime el nombre completo y la edad de cada persona.