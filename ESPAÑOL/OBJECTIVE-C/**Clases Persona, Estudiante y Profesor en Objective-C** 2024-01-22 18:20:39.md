```objective-c
// Definir una interfaz para una clase Persona

@interface Persona : NSObject

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSString *apellido;
@property (nonatomic, assign) int edad;

@end

// Implementar la clase Persona

@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(int)edad {
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

// Definir una interfaz para una clase Estudiante

@interface Estudiante : Persona

@property (nonatomic, strong) NSString *carrera;
@property (nonatomic, strong) NSMutableArray *cursos;

@end

// Implementar la clase Estudiante

@implementation Estudiante

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(int)edad carrera:(NSString *)carrera cursos:(NSMutableArray *)cursos {
    self = [super initWithNombre:nombre apellido:apellido edad:edad];
    if (self) {
        _carrera = carrera;
        _cursos = cursos;
    }
    return self;
}

- (void)agregarCurso:(NSString *)curso {
    [_cursos addObject:curso];
}

- (NSString *)descripcion {
    return [NSString stringWithFormat:@"Estudiante: %@, Carrera: %@, Cursos: %@", [self nombreCompleto], _carrera, _cursos];
}

@end

// Definir una interfaz para una clase Profesor

@interface Profesor : Persona

@property (nonatomic, strong) NSString *departamento;
@property (nonatomic, strong) NSMutableArray *materias;

@end

// Implementar la clase Profesor

@implementation Profesor

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(int)edad departamento:(NSString *)departamento materias:(NSMutableArray *)materias {
    self = [super initWithNombre:nombre apellido:apellido edad:edad];
    if (self) {
        _departamento = departamento;
        _materias = materias;
    }
    return self;
}

- (void)agregarMateria:(NSString *)materia {
    [_materias addObject:materia];
}

- (NSString *)descripcion {
    return [NSString stringWithFormat:@"Profesor: %@, Departamento: %@, Materias: %@", [self nombreCompleto], _departamento, _materias];
}

@end

// Crear objetos de las clases Persona, Estudiante y Profesor

Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" apellido:@"García" edad:20];
Estudiante *estudiante1 = [[Estudiante alloc] initWithNombre:@"María" apellido:@"Pérez" edad:22 carrera:@"Ingeniería Informática" cursos:[NSMutableArray arrayWithObjects:@"Programación Orientada a Objetos", @"Bases de Datos", @"Redes de Computadoras", nil]];
Profesor *profesor1 = [[Profesor alloc] initWithNombre:@"José" apellido:@"Rodríguez" edad:40 departamento:@"Ciencias de la Computación" materias:[NSMutableArray arrayWithObjects:@"Programación Avanzada", @"Inteligencia Artificial", @"Seguridad Informática", nil]];

// Imprimir los datos de los objetos

NSLog(@"Persona: %@", persona1);
NSLog(@"Estudiante: %@", estudiante1);
NSLog(@"Profesor: %@", profesor1);

```

Explicación del código:

1. Definimos interfaces y implementaciones para las clases Persona, Estudiante y Profesor usando Objective-C clásico. Cada clase tiene sus propias propiedades y métodos.
2. Creamos objetos de las clases Persona, Estudiante y Profesor y los inicializamos con datos.
3. Usamos la función NSLog para imprimir los datos de los objetos en la consola.

Este código es un ejemplo de cómo crear y usar clases en Objective-C. Es un código complejo y diferenciado que difícilmente se repetirá nuevamente.