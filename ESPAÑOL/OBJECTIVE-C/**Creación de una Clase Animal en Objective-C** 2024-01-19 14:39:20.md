```objective-c
#import <Foundation/Foundation.h>

// Definición de una clase
@interface Animal : NSObject

// Propiedades
@property NSString *nombre;
@property int edad;

// Métodos
- (void)comer;
- (void)dormir;

@end

// Implementación de la clase
@implementation Animal

// Método comer
- (void)comer {
    NSLog(@"%@ está comiendo.", self.nombre);
}

// Método dormir
- (void)dormir {
    NSLog(@"%@ está durmiendo.", self.nombre);
}

@end

// Clase principal
int main() {
    // Creación de un objeto de la clase Animal
    Animal *miMascota = [[Animal alloc] init];

    // Asignación de valores a las propiedades del objeto
    miMascota.nombre = @"Toby";
    miMascota.edad = 5;

    // Llamada a los métodos del objeto
    [miMascota comer];
    [miMascota dormir];

    // Liberación de la memoria ocupada por el objeto
    [miMascota release];

    return 0;
}
```

Explicación:

* El código define una clase llamada `Animal` que tiene dos propiedades: `nombre` y `edad`, y dos métodos: `comer` y `dormir`.
* La clase principal (`main`) crea un objeto de la clase `Animal`, asigna valores a sus propiedades y llama a sus métodos.
* El método `comer` imprime el nombre del animal seguido de la palabra "está comiendo".
* El método `dormir` imprime el nombre del animal seguido de la palabra "está durmiendo".
* Cuando el programa termina, el objeto `miMascota` se libera de la memoria.