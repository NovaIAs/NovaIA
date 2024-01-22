```objective-c
#import <Foundation/Foundation.h>

typedef enum: NSUInteger {
    PersonaEstadoSoltero,
    PersonaEstadoCasado,
    PersonaEstadoViudo,
    PersonaEstadoDivorciado
} PersonaEstado;

@interface Persona : NSObject
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSString *apellido;
@property (nonatomic, strong) NSDate *fechaNacimiento;
@property (nonatomic, assign) PersonaEstado estadoCivil;
@end

@implementation Persona
- (instancetype)init {
    self = [super init];
    if (self) {
        _nombre = nil;
        _apellido = nil;
        _fechaNacimiento = nil;
        _estadoCivil = PersonaEstadoSoltero;
    }
    return self;
}

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido fechaNacimiento:(NSDate *)fechaNacimiento estadoCivil:(PersonaEstado)estadoCivil {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _apellido = apellido;
        _fechaNacimiento = fechaNacimiento;
        _estadoCivil = estadoCivil;
    }
    return self;
}

- (NSString *)nombreCompleto {
    return [NSString stringWithFormat:@"%@ %@", _nombre, _apellido];
}

- (NSInteger)edad {
    NSCalendar *calendario = [NSCalendar currentCalendar];
    NSDateComponents *componentesEdad = [calendario components:NSCalendarUnitYear fromDate:_fechaNacimiento toDate:[NSDate date] options:0];
    return componentesEdad.year;
}

- (NSString *)descripcionEstadoCivil {
    switch (_estadoCivil) {
        case PersonaEstadoSoltero:
            return @"Soltero";
        case PersonaEstadoCasado:
            return @"Casado";
        case PersonaEstadoViudo:
            return @"Viudo";
        case PersonaEstadoDivorciado:
            return @"Divorciado";
        default:
            return nil;
    }
}

- (NSString *)descripcion {
    return [NSString stringWithFormat:@"Persona: %@\nNombre: %@\nApellido: %@\nFecha de nacimiento: %@\nEstado civil: %@", self, _nombre, _apellido, _fechaNacimiento, [self descripcionEstadoCivil]];
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Persona *juan = [[Persona alloc] initWithNombre:@"Juan" apellido:@"García" fechaNacimiento:[NSDate dateWithTimeIntervalSince1970:0] estadoCivil:PersonaEstadoSoltero];
        Persona *maría = [[Persona alloc] initWithNombre:@"María" apellido:@"López" fechaNacimiento:[NSDate dateWithTimeIntervalSince1970:0] estadoCivil:PersonaEstadoCasado];
        Persona *pedro = [[Persona alloc] initWithNombre:@"Pedro" apellido:@"Sánchez" fechaNacimiento:[NSDate dateWithTimeIntervalSince1970:0] estadoCivil:PersonaEstadoViudo];
        Persona *ana = [[Persona alloc] initWithNombre:@"Ana" apellido:@"Fernández" fechaNacimiento:[NSDate dateWithTimeIntervalSince1970:0] estadoCivil:PersonaEstadoDivorciado];

        NSLog(@"%@", juan);
        NSLog(@"%@", maría);
        NSLog(@"%@", pedro);
        NSLog(@"%@", ana);
    }
    return 0;
}
```

Explicación del código:

1. Creamos una enumeración `PersonaEstado` para representar los posibles estados civiles de una persona.
2. Creamos una clase `Persona` que representa a una persona y tiene las propiedades `nombre`, `apellido`, `fechaNacimiento` y `estadoCivil`.
3. Implementamos los métodos `init`, `initWithNombre:apellido:fechaNacimiento:estadoCivil:`, `nombreCompleto`, `edad`, `descripcionEstadoCivil` y `descripcion`.
4. Creamos cuatro objetos de la clase `Persona` con diferentes valores para las propiedades.
5. Imprimimos la descripción de los cuatro objetos.

El código es complejo porque:

* Utiliza una enumeración.
* Utiliza una clase con múltiples propiedades.
* Implementa varios métodos.
* Crea varios objetos de una clase.
* Utiliza la consola para imprimir la descripción de los objetos.

Este código es un buen ejemplo de cómo utilizar Objective-C para crear clases y objetos.