```objective-c
#import <Foundation/Foundation.h>

// Clase Persona
@interface Persona : NSObject

// Propiedades
@property NSString *nombre;
@property int edad;

// Métodos
- (void)saludar;

@end

// Implementación de la clase Persona
@implementation Persona

// Método para saludar
- (void)saludar {
    NSLog(@"Hola, mi nombre es %@ y tengo %d años.", self.nombre, self.edad);
}

@end

// Clase Estudiante
@interface Estudiante : Persona

// Propiedades
@property NSString *carrera;
@property float promedio;

// Métodos
- (void)estudiar;

@end

// Implementación de la clase Estudiante
@implementation Estudiante

// Método para estudiar
- (void)estudiar {
    NSLog(@"Estoy estudiando %@ y mi promedio es de %.2f.", self.carrera, self.promedio);
}

@end

// Clase Profesor
@interface Profesor : Persona

// Propiedades
@property NSString *materia;
@property int añosDeExperiencia;

// Métodos
- (void)enseñar;

@end

// Implementación de la clase Profesor
@implementation Profesor

// Método para enseñar
- (void)enseñar {
    NSLog(@"Estoy enseñando %@ y tengo %d años de experiencia.", self.materia, self.añosDeExperiencia);
}

@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creamos un objeto de la clase Persona
        Persona *persona = [[Persona alloc] init];
        persona.nombre = @"Juan";
        persona.edad = 20;

        // Llamamos al método saludar del objeto persona
        [persona saludar];

        // Creamos un objeto de la clase Estudiante
        Estudiante *estudiante = [[Estudiante alloc] init];
        estudiante.nombre = @"María";
        estudiante.edad = 22;
        estudiante.carrera = @"Ingeniería en Sistemas";
        estudiante.promedio = 9.5;

        // Llamamos al método estudiar del objeto estudiante
        [estudiante estudiar];

        // Creamos un objeto de la clase Profesor
        Profesor *profesor = [[Profesor alloc] init];
        profesor.nombre = @"Pedro";
        profesor.edad = 40;
        profesor.materia = @"Matemáticas";
        profesor.añosDeExperiencia = 15;

        // Llamamos al método enseñar del objeto profesor
        [profesor enseñar];
    }

    return 0;
}
```

Este código crea tres clases: Persona, Estudiante y Profesor. La clase Persona tiene dos propiedades: nombre y edad, y un método llamado saludar. La clase Estudiante hereda de la clase Persona y tiene dos propiedades adicionales: carrera y promedio, así como un método llamado estudiar. La clase Profesor también hereda de la clase Persona y tiene dos propiedades adicionales: materia y añosDeExperiencia, así como un método llamado enseñar.

En la función principal, creamos un objeto de cada una de las clases y llamamos a sus métodos correspondientes. Esto nos permite imprimir en la consola diferentes mensajes dependiendo del objeto que estemos usando.