```objective-c
#import <Foundation/Foundation.h>

// Definición de la interfaz de la clase Persona
@interface Persona : NSObject

// Propiedades de la clase Persona
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSString *apellido;
@property (nonatomic, assign) NSInteger edad;

// Métodos de la clase Persona
-(void) saludar;

@end

// Implementación de la clase Persona
@implementation Persona

// Método para inicializar la clase Persona
-(instancetype) initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad {
   self = [super init];
   if (self) {
      self.nombre = nombre;
      self.apellido = apellido;
      self.edad = edad;
   }
   return self;
}

// Método para saludar
-(void) saludar {
   NSLog(@"Hola, mi nombre es %@ %@ y tengo %ld años", self.nombre, self.apellido, self.edad);
}

@end

// Definición de la interfaz de la clase Estudiante
@interface Estudiante : Persona

// Propiedades adicionales de la clase Estudiante
@property (nonatomic, strong) NSString *matricula;
@property (nonatomic, strong) NSArray *cursos;

// Métodos adicionales de la clase Estudiante
-(void) estudiar;

@end

// Implementación de la clase Estudiante
@implementation Estudiante

// Método para inicializar la clase Estudiante
-(instancetype) initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad matricula:(NSString *)matricula cursos:(NSArray *)cursos {
   self = [super initWithNombre:nombre apellido:apellido edad:edad];
   if (self) {
      self.matricula = matricula;
      self.cursos = cursos;
   }
   return self;
}

// Método para estudiar
-(void) estudiar {
   NSLog(@"Estoy estudiando los siguientes cursos:");
   for (NSString *curso in self.cursos) {
      NSLog(@"%@", curso);
   }
}

@end

// Definición de la interfaz de la clase Profesor
@interface Profesor : Persona

// Propiedades adicionales de la clase Profesor
@property (nonatomic, strong) NSString *departamento;
@property (nonatomic, strong) NSArray *materias;

// Métodos adicionales de la clase Profesor
-(void) enseñar;

@end

// Implementación de la clase Profesor
@implementation Profesor

// Método para inicializar la clase Profesor
-(instancetype) initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad departamento:(NSString *)departamento materias:(NSArray *)materias {
   self = [super initWithNombre:nombre apellido:apellido edad:edad];
   if (self) {
      self.departamento = departamento;
      self.materias = materias;
   }
   return self;
}

// Método para enseñar
-(void) enseñar {
   NSLog(@"Estoy enseñando las siguientes materias:");
   for (NSString *materia in self.materias) {
      NSLog(@"%@", materia);
   }
}

@end

// Función principal
int main(int argc, const char * argv[]) {
   @autoreleasepool {
      // Crear objetos de la clase Persona, Estudiante y Profesor
      Persona *persona = [[Persona alloc] initWithNombre:@"Juan" apellido:@"García" edad:25];
      Estudiante *estudiante = [[Estudiante alloc] initWithNombre:@"María" apellido:@"Fernández" edad:20 matricula:@"123456" cursos:@[@"Matemáticas", @"Física", @"Química"]];
      Profesor *profesor = [[Profesor alloc] initWithNombre:@"Pedro" apellido:@"Rodríguez" edad:40 departamento:@"Ciencias" materias:@[@"Biología", "Geología", "Astronomía"]];

      // Llamar a los métodos de los objetos
      [persona saludar];
      [estudiante estudiar];
      [profesor enseñar];
   }
   return 0;
}
```