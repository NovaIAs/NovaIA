```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject

@property (nonatomic, copy) NSString *nombre;
@property (nonatomic, copy) NSString *apellido;
@property (nonatomic, assign) NSInteger edad;

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad;

@end

@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _apellido = apellido;
        _edad = edad;
    }
    return self;
}

@end

@interface Empleado : Persona

@property (nonatomic, copy) NSString *puesto;
@property (nonatomic, assign) NSInteger salario;

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad puesto:(NSString *)puesto salario:(NSInteger)salario;

@end

@implementation Empleado

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad puesto:(NSString *)puesto salario:(NSInteger)salario {
    self = [super initWithNombre:nombre apellido:apellido edad:edad];
    if (self) {
        _puesto = puesto;
        _salario = salario;
    }
    return self;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear una instancia de la clase Persona
        Persona *persona = [[Persona alloc] initWithNombre:@"Juan" apellido:@"García" edad:30];
        
        // Imprimir el nombre y la edad de la persona
        NSLog(@"Nombre: %@, Edad: %ld", persona.nombre, persona.edad);
        
        // Crear una instancia de la clase Empleado
        Empleado *empleado = [[Empleado alloc] initWithNombre:@"María" apellido:@"López" edad:40 puesto:@"Ingeniero de Software" salario:5000];
        
        // Imprimir el nombre, la edad, el puesto y el salario del empleado
        NSLog(@"Nombre: %@, Edad: %ld, Puesto: %@, Salario: %ld", empleado.nombre, empleado.edad, empleado.puesto, empleado.salario);
    }
    return 0;
}
```

Explicación del código:

1. Definimos una clase `Persona` que tiene tres propiedades: `nombre`, `apellido` y `edad`. También definimos un método `initWithNombre:apellido:edad:` para crear una instancia de la clase `Persona`.

2. Definimos una clase `Empleado` que hereda de la clase `Persona`. La clase `Empleado` tiene dos propiedades adicionales: `puesto` y `salario`. También definimos un método `initWithNombre:apellido:edad:puesto:salario:` para crear una instancia de la clase `Empleado`.

3. En la función `main()`, creamos una instancia de la clase `Persona` y otra de la clase `Empleado`. A continuación, imprimimos el nombre, la edad, el puesto y el salario de la persona y del empleado utilizando el método `NSLog()`.

Este código demuestra cómo crear clases y objetos en Objective-C. También muestra cómo usar herencia y métodos para crear objetos con diferentes propiedades y comportamientos.