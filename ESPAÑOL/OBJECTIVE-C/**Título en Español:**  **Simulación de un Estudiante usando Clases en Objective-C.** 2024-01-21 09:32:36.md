```objective-c
#import <Foundation/Foundation.h>

// Define una clase llamada "Estudiante"
@interface Estudiante : NSObject

// Propiedades de la clase
@property NSString *nombre;
@property NSInteger edad;
@property NSString *carrera;

// Constructor de la clase
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad carrera:(NSString *)carrera;

// Métodos de la clase
- (void)estudiar;
- (void)descansar;

@end

// Implementación de la clase "Estudiante"
@implementation Estudiante

// Constructor de la clase
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad carrera:(NSString *)carrera {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
        self.carrera = carrera;
    }
    return self;
}

// Método "estudiar"
- (void)estudiar {
    NSLog(@"El estudiante %@ está estudiando.", self.nombre);
}

// Método "descansar"
- (void)descansar {
    NSLog(@"El estudiante %@ está descansando.", self.nombre);
}

@end

// Clase principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crea una instancia de la clase "Estudiante"
        Estudiante *estudiante = [[Estudiante alloc] initWithNombre:@"Juan" edad:20 carrera:@"Ingeniería Informática"];
        
        // Llama al método "estudiar"
        [estudiante estudiar];
        
        // Llama al método "descansar"
        [estudiante descansar];
    }
    return 0;
}
```

Explicación del código:

1. **Clase "Estudiante"**: Se define una clase llamada "Estudiante" que representa a un estudiante con propiedades como nombre, edad y carrera.

2. **Constructor de la Clase**: Se define un constructor para la clase "Estudiante" que inicializa las propiedades al crear una nueva instancia de la clase.

3. **Métodos de la Clase**: Se definen dos métodos para la clase "Estudiante": "estudiar" y "descansar", que representan las acciones que puede realizar un estudiante.

4. **Clase Principal**: Se define la función `main()` que es el punto de entrada del programa.

5. **Creación de una Instancia de la Clase "Estudiante"**: Dentro de la función `main()`, se crea una instancia de la clase "Estudiante" llamada "estudiante" utilizando el constructor definido anteriormente.

6. **Llamadas a Métodos de la Clase**: Después de crear la instancia "estudiante", se llaman a los métodos "estudiar" y "descansar" para simular las acciones que puede realizar un estudiante.

Este código es más complejo que el anterior porque incluye una clase definida por el usuario con propiedades, constructor y métodos, lo que muestra una comprensión más profunda de los fundamentos del lenguaje Objective-C.