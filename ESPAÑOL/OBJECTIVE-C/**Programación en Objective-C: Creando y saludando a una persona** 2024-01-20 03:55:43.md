```objective-c
#import <Foundation/Foundation.h>

// Definimos una interfaz para la clase Persona
@interface Persona : NSObject

// Propiedades de la clase Persona
@property NSString *nombre;
@property NSInteger edad;

// Métodos de la clase Persona
- (void)saludar;

@end

// Implementación de la clase Persona
@implementation Persona

// Método que saluda a la persona
- (void)saludar {
    NSLog(@"Hola, mi nombre es %@ y tengo %ld años.", self.nombre, self.edad);
}

@end

// Función principal del programa
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creamos un objeto de la clase Persona
        Persona *persona = [[Persona alloc] init];
        
        // Establecemos el nombre y la edad de la persona
        persona.nombre = @"Juan";
        persona.edad = 25;
        
        // Llamamos al método saludar de la persona
        [persona saludar];
    }
    
    return 0;
}
```

Explicación del código:

* Primero, importamos la biblioteca Foundation.h, que contiene las clases y funciones básicas del lenguaje Objective-C.
* Definimos una interfaz para la clase Persona, que incluye las propiedades y métodos de la clase.
* Implementamos la clase Persona, que proporciona código para los métodos definidos en la interfaz.
* En la función principal del programa, creamos un objeto de la clase Persona, establecemos sus propiedades y llamamos a su método saludar.