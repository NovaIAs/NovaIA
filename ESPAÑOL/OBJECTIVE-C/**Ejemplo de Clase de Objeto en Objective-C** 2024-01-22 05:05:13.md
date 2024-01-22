```objective-c
#import <UIKit/UIKit.h>
#import <Foundation/Foundation.h>

@interface Ejemplo : NSObject {
    NSString *nombre;
    NSInteger edad;
    NSMutableArray *hobbies;
}

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) NSInteger edad;
@property (nonatomic, strong) NSMutableArray *hobbies;

- (id)initWithNombre:(NSString *)nombre yEdad:(NSInteger)edad;

@end

@implementation Ejemplo

- (id)initWithNombre:(NSString *)nombre yEdad:(NSInteger)edad {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _edad = edad;
        _hobbies = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)addHobby:(NSString *)hobby {
    [_hobbies addObject:hobby];
}

- (NSString *)description {
    return [NSString stringWithFormat:
            @"Nombre: %@\n"
            @"Edad: %ld\n"
            @"Hobbies: %@",
            _nombre,
            _edad,
            _hobbies
            ];
}

@end

int main(int argc, char * argv[]) {
    @autoreleasepool {
        Ejemplo *ejemplo = [[Ejemplo alloc] initWithNombre:@"Juan" yEdad:20];
        
        [ejemplo addHobby:@"Programar"];
        [ejemplo addHobby:@"Leer"];
        [ejemplo addHobby:@"Viajar"];
        
        NSLog(@"%@", [ejemplo description]);
    }
    return 0;
}
```

Explicación del código:

1. Declaramos la clase `Ejemplo` que hereda de `NSObject`.

2. En la interfaz de la clase `Ejemplo`, definimos las propiedades `nombre`, `edad` y `hobbies`.

3. En la implementación de la clase `Ejemplo`, definimos un método inicializador que recibe el nombre y la edad de la persona.

4. También definimos un método `addHobby` que añade un nuevo hobby a la lista de hobbies.

5. El método `description` devuelve una descripción de la persona en formato de cadena de caracteres.

6. En la función `main`, creamos un objeto de tipo `Ejemplo` con el nombre "Juan" y la edad 20.

7. Añadimos los hobbies "Programar", "Leer" y "Viajar" al objeto.

8. Finalmente, imprimimos por consola la descripción de la persona.