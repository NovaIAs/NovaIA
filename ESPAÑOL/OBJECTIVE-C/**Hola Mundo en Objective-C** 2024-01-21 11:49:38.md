```objective-c
#import <Foundation/Foundation.h>

@interface MiClase : NSObject {
    NSString *nombre;
    int edad;
}

@property (nonatomic, copy) NSString *nombre;
@property (nonatomic, assign) int edad;

- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad;
- (void)saludar;

@end

@implementation MiClase

- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

- (void)saludar {
    NSLog(@"Hola, mi nombre es %@ y tengo %d años.", self.nombre, self.edad);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        MiClase *persona1 = [[MiClase alloc] initWithNombre:@"Juan" andEdad:25];
        MiClase *persona2 = [[MiClase alloc] initWithNombre:@"María" andEdad:30];

        [persona1 saludar];
        [persona2 saludar];
    }
    return 0;
}
```

Explicación:

El código crea una clase llamada "MiClase" con dos propiedades, "nombre" y "edad". Un método "init" que inicializa las propiedades y un método "saludar" que imprime el nombre y la edad del objeto.

En la función "main", se crean dos instancias de "MiClase", "persona1" y "persona2". Se asignan sus propiedades y luego se llama al método "saludar" para cada una.

El código utiliza varios conceptos importantes de Objective-C, incluyendo:

- **Objetos y clases:** Una clase es una plantilla que define las propiedades y métodos de un tipo de objeto. Una instancia de una clase es un objeto que tiene las propiedades y métodos definidos en la clase.
- **Propiedades:** Una propiedad es un atributo de un objeto. Puede ser de lectura y escritura, de solo lectura o de solo escritura.
- **Métodos:** Un método es una función que puede ser llamada en un objeto.
- **Mensajes:** Cuando se llama a un método en un objeto, se enviando un mensaje al objeto. El método es entonces ejecutado por el objeto.
- **Delegación:** La delegación es una forma de comunicación entre objetos. Un objeto puede delegar una tarea a otro objeto. El objeto delegado es responsable de realizar la tarea.
- **Bloques:** Un bloque es un código que puede ser pasado como un parámetro a otro método. Los bloques pueden ser usados para encapsular código que puede ser llamado más tarde.

Este código es solo un ejemplo de cómo usar Objective-C. Hay muchos otros conceptos y características que no se cubren en este código.