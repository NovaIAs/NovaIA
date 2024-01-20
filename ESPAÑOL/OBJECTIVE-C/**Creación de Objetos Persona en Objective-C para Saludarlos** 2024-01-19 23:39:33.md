```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) NSInteger edad;

- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad;

- (void)saludar;

@end

@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

- (void)saludar {
    NSLog(@"Hola, mi nombre es %@ y tengo %ld años.", self.nombre, self.edad);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:25];
        Persona *persona2 = [[Persona alloc] initWithNombre:@"María" edad:30];

        [persona1 saludar];
        [persona2 saludar];
    }
    return 0;
}
```

Este código crea una clase llamada `Persona` que tiene dos propiedades: `nombre` y `edad`. La clase `Persona` también tiene un método llamado `saludar` que imprime un mensaje de saludo con el nombre y la edad de la persona.

En la función `main`, se crean dos objetos `Persona` llamados `persona1` y `persona2`. Luego, se llama al método `saludar` de cada objeto para imprimir un mensaje de saludo.

El código utiliza Objective-C, que es un lenguaje de programación orientado a objetos que se utiliza para desarrollar aplicaciones para iOS, macOS y watchOS.