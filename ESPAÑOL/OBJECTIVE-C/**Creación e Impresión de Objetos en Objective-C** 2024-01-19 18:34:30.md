```objective-c
// Biblioteca de importación
#import <Foundation/Foundation.h>

// Clase personalizada
@interface Persona : NSObject {
    NSString *nombre;
    int edad;
}

// Métodos de la clase
- (id)initWithNombre:(NSString *)nombre edad:(int)edad; // Constructor
- (NSString *)nombre; // Nombre de la persona
- (int)edad; // Edad de la persona
- (void)setNombre:(NSString *)nombre; // Establecer el nombre
- (void)setEdad:(int)edad; // Establecer la edad

@end

// Implementación de la clase
@implementation Persona

// Constructor
- (id)initWithNombre:(NSString *)nombre edad:(int)edad {
    self = [super init]; // Llama al constructor de la clase padre
    if (self) {
        self.nombre = nombre; // Establece el nombre
        self.edad = edad; // Establece la edad
    }
    return self;
}

// Métodos get y set
- (NSString *)nombre {
    return nombre; // Devuelve el nombre
}

- (int)edad {
    return edad; // Devuelve la edad
}

- (void)setNombre:(NSString *)nombre {
    self->nombre = nombre; // Establece el nombre
}

- (void)setEdad:(int)edad {
    self->edad = edad; // Establece la edad
}

@end

// Función principal
int main() {
    // Crear un nuevo objeto de tipo "Persona"
    Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:25];

    // Imprimir el nombre y la edad de la persona
    NSLog(@"Nombre: %@", persona1.nombre);
    NSLog(@"Edad: %d", persona1.edad);

    // Cambiar el nombre y la edad de la persona
    persona1.nombre = @"María";
    persona1.edad = 30;

    // Imprimir nuevamente el nombre y la edad de la persona
    NSLog(@"Nombre: %@", persona1.nombre);
    NSLog(@"Edad: %d", persona1.edad);

    // Devolver 0 al sistema operativo para indicar que el programa ha finalizado correctamente
    return 0;
}
```

Explicación del código:

- Se crea una clase personalizada llamada "Persona" que representa a una persona con un nombre y una edad.
- La clase "Persona" tiene dos métodos de instancia llamados "nombre" y "edad" que devuelven el nombre y la edad de la persona, respectivamente.
- La clase "Persona" también tiene dos métodos de instancia llamados "setNombre" y "setEdad" que establecen el nombre y la edad de la persona, respectivamente.
- En la función "main", se crea un nuevo objeto de tipo "Persona" llamado "persona1" con el nombre "Juan" y la edad 25.
- Se imprimen el nombre y la edad de "persona1" utilizando los métodos "nombre" y "edad".
- Se cambian el nombre y la edad de "persona1" a "María" y 30, respectivamente, utilizando los métodos "setNombre" y "setEdad".
- Se imprimen nuevamente el nombre y la edad de "persona1" utilizando los métodos "nombre" y "edad".
- El programa devuelve 0 al sistema operativo para indicar que ha finalizado correctamente.