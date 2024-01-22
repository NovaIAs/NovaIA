```objective-c
// Este código crea una clase llamada 'Persona' con propiedades y métodos.

#import <Foundation/Foundation.h>

@interface Persona : NSObject

// Propiedades
@property (strong, nonatomic) NSString *nombre;
@property (strong, nonatomic) NSString *apellido;
@property (assign, nonatomic) NSInteger edad;

// Métodos
- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad;
- (NSString *)nombreCompleto;

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

- (NSString *)nombreCompleto {
    return [NSString stringWithFormat:@"%@ %@", _nombre, _apellido];
}

@end

// Este código crea una clase llamada 'Estudiante' que hereda de 'Persona'.

#import "Persona.h"

@interface Estudiante : Persona

// Propiedades
@property (strong, nonatomic) NSString *matricula;
@property (strong, nonatomic) NSArray *materias;

// Métodos
- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad matricula:(NSString *)matricula materias:(NSArray *)materias;

@end

@implementation Estudiante

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad matricula:(NSString *)matricula materias:(NSArray *)materias {
    self = [super initWithNombre:nombre apellido:apellido edad:edad];
    if (self) {
        _matricula = matricula;
        _materias = materias;
    }
    return self;
}

@end

// Este código crea una clase llamada 'Profesor' que hereda de 'Persona'.

#import "Persona.h"

@interface Profesor : Persona

// Propiedades
@property (strong, nonatomic) NSString *departamento;
@property (strong, nonatomic) NSArray *cursos;

// Métodos
- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad departamento:(NSString *)departamento cursos:(NSArray *)cursos;

@end

@implementation Profesor

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad departamento:(NSString *)departamento cursos:(NSArray *)cursos {
    self = [super initWithNombre:nombre apellido:apellido edad:edad];
    if (self) {
        _departamento = departamento;
        _cursos = cursos;
    }
    return self;
}

@end

// Este código crea una instancia de la clase 'Persona' con el nombre "Juan", el apellido "García" y la edad 25.

Persona *persona = [[Persona alloc] initWithNombre:@"Juan" apellido:@"García" edad:25];

// Este código crea una instancia de la clase 'Estudiante' con el nombre "María", el apellido "López", la edad 20, la matrícula "123456" y las materias ["Matemáticas", "Física", "Química"].

Estudiante *estudiante = [[Estudiante alloc] initWithNombre:@"María" apellido:@"López" edad:20 matricula:@"123456" materias:@[@"Matemáticas", @"Física", @"Química"]];

// Este código crea una instancia de la clase 'Profesor' con el nombre "Pedro", el apellido "Sánchez", la edad 35, el departamento "Ciencias de la Computación" y los cursos ["Programación", "Bases de Datos", "Inteligencia Artificial"].

Profesor *profesor = [[Profesor alloc] initWithNombre:@"Pedro" apellido:@"Sánchez" edad:35 departamento:@"Ciencias de la Computación" cursos:@[@"Programación", @"Bases de Datos", @"Inteligencia Artificial"]];

// Este código imprime el nombre completo de la persona, el nombreCompleto del estudiante y el nombreCompleto del profesor.

NSLog(@"Nombre completo de la persona: %@", persona.nombreCompleto);
NSLog(@"Nombre completo del estudiante: %@", estudiante.nombreCompleto);
NSLog(@"Nombre completo del profesor: %@", profesor.nombreCompleto);
```

Explicación del código:

1. Creamos una clase llamada 'Persona' con propiedades para el nombre, el apellido y la edad. También creamos métodos para inicializar una instancia de la clase y para obtener el nombre completo de la persona.

2. Creamos una clase llamada 'Estudiante' que hereda de 'Persona'. Agregamos propiedades para la matrícula y las materias. También creamos un método para inicializar una instancia de la clase.

3. Creamos una clase llamada 'Profesor' que hereda de 'Persona'. Agregamos propiedades para el departamento y los cursos. También creamos un método para inicializar una instancia de la clase.

4. Creamos una instancia de la clase 'Persona' con el nombre "Juan", el apellido "García" y la edad 25.

5. Creamos una instancia de la clase 'Estudiante' con el nombre "María", el apellido "López", la edad 20, la matrícula "123456" y las materias ["Matemáticas", "Física", "Química"].

6. Creamos una instancia de la clase 'Profesor' con el nombre "Pedro", el apellido "Sánchez", la edad 35, el departamento "Ciencias de la Computación" y los cursos ["Programación", "Bases de Datos", "Inteligencia Artificial"].

7. Imprimimos el nombre completo de la persona, el nombreCompleto del estudiante y el nombreCompleto del profesor.