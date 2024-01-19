```objective-c
// Importamos la librería Foundation.
#import <Foundation/Foundation.h>

// Creamos una clase llamada "Persona".
@interface Persona : NSObject

// Propiedades de la clase.
@property NSString *nombre;
@property int edad;

// Método para inicializar la clase.
- (id)initWithNombre:(NSString *)nombre edad:(int)edad;

// Método para obtener el nombre de la persona.
- (NSString *)getNombre;

// Método para obtener la edad de la persona.
- (int)getEdad;

@end

// Implementamos la clase "Persona".
@implementation Persona

// Método para inicializar la clase.
- (id)initWithNombre:(NSString *)nombre edad:(int)edad {
  if (self = [super init]) {
    _nombre = nombre;
    _edad = edad;
  }
  return self;
}

// Método para obtener el nombre de la persona.
- (NSString *)getNombre {
  return _nombre;
}

// Método para obtener la edad de la persona.
- (int)getEdad {
  return _edad;
}

@end

// Creamos una clase llamada "Estudiante".
@interface Estudiante : Persona

// Propiedades de la clase.
@property NSString *matricula;
@property NSString *carrera;

// Método para inicializar la clase.
- (id)initWithNombre:(NSString *)nombre edad:(int)edad matricula:(NSString *)matricula carrera:(NSString *)carrera;

// Método para obtener la matrícula del estudiante.
- (NSString *)getMatricula;

// Método para obtener la carrera del estudiante.
- (NSString *)getCarrera;

@end

// Implementamos la clase "Estudiante".
@implementation Estudiante

// Método para inicializar la clase.
- (id)initWithNombre:(NSString *)nombre edad:(int)edad matricula:(NSString *)matricula carrera:(NSString *)carrera {
  if (self = [super initWithNombre:nombre edad:edad]) {
    _matricula = matricula;
    _carrera = carrera;
  }
  return self;
}

// Método para obtener la matrícula del estudiante.
- (NSString *)getMatricula {
  return _matricula;
}

// Método para obtener la carrera del estudiante.
- (NSString *)getCarrera {
  return _carrera;
}

@end

int main(int argc, const char * argv[]) {
  @autoreleasepool {
    // Creamos un objeto de la clase "Estudiante".
    Estudiante *estudiante = [[Estudiante alloc] initWithNombre:@"Juan" edad:20 matricula:@"123456" carrera:@"Ingeniería en Informática"];

    // Imprimimos el nombre del estudiante.
    NSLog(@"Nombre: %@", [estudiante getNombre]);

    // Imprimimos la edad del estudiante.
    NSLog(@"Edad: %d", [estudiante getEdad]);

    // Imprimimos la matrícula del estudiante.
    NSLog(@"Matrícula: %@", [estudiante getMatricula]);

    // Imprimimos la carrera del estudiante.
    NSLog(@"Carrera: %@", [estudiante getCarrera]);
  }
  return 0;
}
```

Explicación del código:

* Importamos la librería Foundation, que proporciona las clases y los métodos básicos para el desarrollo de aplicaciones en Objective-C.
* Creamos una clase llamada "Persona" que define las propiedades y los métodos comunes a todas las personas.
* Implementamos la clase "Persona" definiendo los métodos para inicializar la clase, obtener el nombre de la persona y obtener la edad de la persona.
* Creamos una clase llamada "Estudiante" que hereda de la clase "Persona" y define las propiedades y los métodos específicos de los estudiantes.
* Implementamos la clase "Estudiante" definiendo los métodos para inicializar la clase, obtener la matrícula del estudiante, obtener la carrera del estudiante y obtener el nombre y la edad del estudiante (heredados de la clase "Persona").
* En la función `main`, creamos un objeto de la clase "Estudiante", imprimimos sus propiedades y liberamos la memoria ocupada por el objeto.