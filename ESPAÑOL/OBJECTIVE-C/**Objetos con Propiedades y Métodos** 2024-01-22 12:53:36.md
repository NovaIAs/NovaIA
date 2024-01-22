```objective-c
#import <Foundation/Foundation.h>

// Definición de la clase Persona
@interface Persona : NSObject
{
    NSString *_nombre;
    int _edad;
}

// Propiedades
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) int edad;

// Método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad;

// Métodos de instancia
- (void)saludar;
- (void)caminar:(int)pasos;

@end

// Implementación de la clase Persona
@implementation Persona

// Método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad
{
    self = [super init];
    if (self) {
        _nombre = nombre;
        _edad = edad;
    }
    return self;
}

// Métodos de instancia
- (void)saludar
{
    NSLog(@"Hola, mi nombre es %@ y tengo %d años", _nombre, _edad);
}

- (void)caminar:(int)pasos
{
    NSLog(@"Estoy caminando %d pasos", pasos);
}

@end

// Definición de la clase Estudiante
@interface Estudiante : Persona
{
    NSString *_matricula;
    NSString *_carrera;
}

// Propiedades
@property (nonatomic, strong) NSString *matricula;
@property (nonatomic, strong) NSString *carrera;

// Método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad matricula:(NSString *)matricula carrera:(NSString *)carrera;

// Métodos de instancia
- (void)estudiar:(int)horas;
- (void)presentarExamen;

@end

// Implementación de la clase Estudiante
@implementation Estudiante

// Método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad matricula:(NSString *)matricula carrera:(NSString *)carrera
{
    self = [super initWithNombre:nombre edad:edad];
    if (self) {
        _matricula = matricula;
        _carrera = carrera;
    }
    return self;
}

// Métodos de instancia
- (void)estudiar:(int)horas
{
    NSLog(@"Estoy estudiando %d horas", horas);
}

- (void)presentarExamen
{
    NSLog(@"Estoy presentando un examen");
}

@end

// Definición de la clase Profesor
@interface Profesor : Persona
{
    NSString *_titulo;
    NSString *_departamento;
}

// Propiedades
@property (nonatomic, strong) NSString *titulo;
@property (nonatomic, strong) NSString *departamento;

// Método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad titulo:(NSString *)titulo departamento:(NSString *)departamento;

// Métodos de instancia
- (void)enseñar:(NSString *)materia;
- (void)calificarExamenes;

@end

// Implementación de la clase Profesor
@implementation Profesor

// Método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad titulo:(NSString *)titulo departamento:(NSString *)departamento
{
    self = [super initWithNombre:nombre edad:edad];
    if (self) {
        _titulo = titulo;
        _departamento = departamento;
    }
    return self;
}

// Métodos de instancia
- (void)enseñar:(NSString *)materia
{
    NSLog(@"Estoy enseñando %@ materia", materia);
}

- (void)calificarExamenes
{
    NSLog(@"Estoy calificando exámenes");
}

@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Crear una instancia de la clase Persona
        Persona *persona = [[Persona alloc] initWithNombre:@"Juan" edad:20];
        
        // Crear una instancia de la clase Estudiante
        Estudiante *estudiante = [[Estudiante alloc] initWithNombre:@"María" edad:22 matricula:@"123456" carrera:@"Ingeniería en Sistemas"];
        
        // Crear una instancia de la clase Profesor
        Profesor *profesor = [[Profesor alloc] initWithNombre:@"Pedro" edad:50 titulo:@"Doctor" departamento:@"Ciencias de la Computación"];
        
        // Llamar a los métodos de las instancias
        [persona saludar];
        [persona caminar:100];
        
        [estudiante estudiar:3];
        [estudiante presentarExamen];
        
        [profesor enseñar:@"Programación" materia:@"C"];
        [profesor calificarExamenes];
    }
    return 0;
}
```

Este es un código complejo en Objective-C que define tres clases: Persona, Estudiante y Profesor. Cada clase tiene sus propias propiedades y métodos de instancia. La clase Persona es la clase base de las clases Estudiante y Profesor.

En la función principal, se crean instancias de las tres clases y se llaman a sus métodos.

Aquí una explicación detallada del código:

* **Clase Persona:**
    * Propiedades: `nombre` y `edad`.
    * Métodos de instancia: `saludar` y `caminar`.
* **Clase Estudiante:**
    * Hereda de la clase Persona.
    * Propiedades: `matricula` y `carrera`.
    * Métodos de instancia: `estudiar` y `presentarExamen`.
* **Clase Profesor:**
    * Hereda de la clase Persona.
    * Propiedades: `titulo` y `departamento`.
    * Métodos de instancia: `enseñar` y `calificarExamenes`.
* **Función principal:**
    * Se crean instancias de las tres clases y se llaman a sus métodos.

Este código es un ejemplo de cómo crear y usar clases en Objective-C. Es un lenguaje relativamente sencillo de aprender y usar, pero puede ser complejo cuando se usan características avanzadas como la herencia y el polimorfismo.