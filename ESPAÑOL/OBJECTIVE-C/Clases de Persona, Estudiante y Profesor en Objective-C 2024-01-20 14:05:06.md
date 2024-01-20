```objective-c
// Creamos una clase llamada "Persona"
@interface Persona : NSObject

// Propiedades de la clase "Persona"
@property NSString *nombre;
@property int edad;

// Método para inicializar una instancia de la clase "Persona"
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad;

// Método para obtener el nombre de la persona
- (NSString *)nombre;

// Método para obtener la edad de la persona
- (int)edad;

// Método para imprimir el nombre y la edad de la persona
- (void)imprimirInfo;

@end

// Implementación de la clase "Persona"
@implementation Persona

// Método para inicializar una instancia de la clase "Persona"
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

// Método para obtener el nombre de la persona
- (NSString *)nombre {
    return self.nombre;
}

// Método para obtener la edad de la persona
- (int)edad {
    return self.edad;
}

// Método para imprimir el nombre y la edad de la persona
- (void)imprimirInfo {
    NSLog(@"Nombre: %@, Edad: %d", self.nombre, self.edad);
}

@end

// Creamos una clase llamada "Estudiante" que hereda de la clase "Persona"
@interface Estudiante : Persona

// Propiedades de la clase "Estudiante"
@property NSString *universidad;
@property NSString *carrera;

// Método para inicializar una instancia de la clase "Estudiante"
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad universidad:(NSString *)universidad carrera:(NSString *)carrera;

// Método para obtener la universidad del estudiante
- (NSString *)universidad;

// Método para obtener la carrera del estudiante
- (NSString *)carrera;

// Método para imprimir la información del estudiante
- (void)imprimirInfoEstudiante;

@end

// Implementación de la clase "Estudiante"
@implementation Estudiante

// Método para inicializar una instancia de la clase "Estudiante"
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad universidad:(NSString *)universidad carrera:(NSString *)carrera {
    self = [super initWithNombre:nombre edad:edad];
    if (self) {
        self.universidad = universidad;
        self.carrera = carrera;
    }
    return self;
}

// Método para obtener la universidad del estudiante
- (NSString *)universidad {
    return self.universidad;
}

// Método para obtener la carrera del estudiante
- (NSString *)carrera {
    return self.carrera;
}

// Método para imprimir la información del estudiante
- (void)imprimirInfoEstudiante {
    NSLog(@"Nombre: %@, Edad: %d, Universidad: %@, Carrera: %@", self.nombre, self.edad, self.universidad, self.carrera);
}

@end

// Creamos una clase llamada "Profesor" que hereda de la clase "Persona"
@interface Profesor : Persona

// Propiedades de la clase "Profesor"
@property NSString *universidad;
@property NSString *departamento;

// Método para inicializar una instancia de la clase "Profesor"
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad universidad:(NSString *)universidad departamento:(NSString *)departamento;

// Método para obtener la universidad del profesor
- (NSString *)universidad;

// Método para obtener el departamento del profesor
- (NSString *)departamento;

// Método para imprimir la información del profesor
- (void)imprimirInfoProfesor;

@end

// Implementación de la clase "Profesor"
@implementation Profesor

// Método para inicializar una instancia de la clase "Profesor"
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad universidad:(NSString *)universidad departamento:(NSString *)departamento {
    self = [super initWithNombre:nombre edad:edad];
    if (self) {
        self.universidad = universidad;
        self.departamento = departamento;
    }
    return self;
}

// Método para obtener la universidad del profesor
- (NSString *)universidad {
    return self.universidad;
}

// Método para obtener el departamento del profesor