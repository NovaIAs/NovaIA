```objective-c
// Importamos la librería Foundation.
#import <Foundation/Foundation.h>

// Definimos la interfaz de una clase llamada "Persona".
@interface Persona : NSObject {
    // Propiedades privadas.
    NSString *nombre;
    int edad;
}

// Métodos públicos.
- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad;
- (NSString *)getNombre;
- (int)getEdad;
- (void)setNombre:(NSString *)nombre;
- (void)setEdad:(int)edad;

@end

// Implementación de la clase "Persona".
@implementation Persona

// Método constructor.
- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad {
    // Inicializamos el objeto.
    self = [super init];
    if (self) {
        // Asignamos los valores a las propiedades.
        self->nombre = nombre;
        self->edad = edad;
    }
    return self;
}

// Método para obtener el nombre.
- (NSString *)getNombre {
    // Devolvemos el nombre.
    return self->nombre;
}

// Método para obtener la edad.
- (int)getEdad {
    // Devolvemos la edad.
    return self->edad;
}

// Método para establecer el nombre.
- (void)setNombre:(NSString *)nombre {
    // Asignamos el nombre.
    self->nombre = nombre;
}

// Método para establecer la edad.
- (void)setEdad:(int)edad {
    // Asignamos la edad.
    self->edad = edad;
}

@end

// Definimos la interfaz de una clase llamada "Estudiante".
@interface Estudiante : Persona {
    // Propiedades privadas.
    float notaMedia;
}

// Métodos públicos.
- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad andNotaMedia:(float)notaMedia;
- (float)getNotaMedia;
- (void)setNotaMedia:(float)notaMedia;

@end

// Implementación de la clase "Estudiante".
@implementation Estudiante

// Método constructor.
- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad andNotaMedia:(float)notaMedia {
    // Inicializamos el objeto.
    self = [super initWithNombre:nombre andEdad:edad];
    if (self) {
        // Asignamos los valores a las propiedades.
        self->notaMedia = notaMedia;
    }
    return self;
}

// Método para obtener la nota media.
- (float)getNotaMedia {
    // Devolvemos la nota media.
    return self->notaMedia;
}

// Método para establecer la nota media.
- (void)setNotaMedia:(float)notaMedia {
    // Asignamos la nota media.
    self->notaMedia = notaMedia;
}

@end

// Función principal.
int main(int argc, const char * argv[]) {
    // Creamos un objeto de tipo "Estudiante".
    Estudiante *estudiante = [[Estudiante alloc] initWithNombre:@"Juan" andEdad:20 andNotaMedia:8.5];

    // Imprimimos el nombre, la edad y la nota media del estudiante.
    NSLog(@"Nombre: %@", [estudiante getNombre]);
    NSLog(@"Edad: %d", [estudiante getEdad]);
    NSLog(@"Nota media: %.2f", [estudiante getNotaMedia]);

    // Liberamos la memoria utilizada por el objeto.
    [estudiante release];

    return 0;
}
```

Explicación del código:

* **Clase `Persona`**: Define una clase llamada `Persona` que representa a una persona genérica. Tiene dos propiedades privadas: `nombre` y `edad`. También tiene métodos públicos para obtener y establecer estas propiedades.
* **Clase `Estudiante`**: Define una clase llamada `Estudiante` que hereda de la clase `Persona`. Tiene una propiedad privada adicional: `notaMedia`. También tiene métodos públicos para obtener y establecer esta propiedad.
* **Función `main`**: Es la función principal del programa. Crea un objeto de tipo `Estudiante`, imprime su nombre, edad y nota media, y luego libera la memoria utilizada por el objeto.

Este código es un ejemplo de cómo crear clases y objetos en Objective-C. Es un lenguaje de programación orientado a objetos, lo que significa que se basa en el concepto de objetos. Un objeto es una entidad que tiene un estado (sus propiedades) y un comportamiento (sus métodos).