```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSString *apellido;
@property (nonatomic, assign) NSInteger edad;

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad;
- (void)saludar;

@end

@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.apellido = apellido;
        self.edad = edad;
    }
    return self;
}

- (void)saludar {
    NSLog(@"Hola, mi nombre es %@ %@ y tengo %ld años", self.nombre, self.apellido, self.edad);
}

@end

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" apellido:@"García" edad:25];
        Persona *persona2 = [[Persona alloc] initWithNombre:@"María" apellido:@"Pérez" edad:30];
        
        [persona1 saludar];
        [persona2 saludar];
    }
    return 0;
}
```

Explicación:

* La clase `Persona` es una clase simple que representa a una persona. Tiene tres propiedades: `nombre`, `apellido` y `edad`.
* Los métodos `initWithNombre:apellido:edad:` y `saludar` son métodos de instancia de la clase `Persona`. El método `initWithNombre:apellido:edad:` es un método inicializador que crea una nueva instancia de la clase `Persona` con los valores especificados para las propiedades `nombre`, `apellido` y `edad`. El método `saludar` es un método que imprime un mensaje de saludo en la consola.
* La función `main` es la función de entrada del programa. Crea dos instancias de la clase `Persona` y llama al método `saludar` en cada una de ellas.