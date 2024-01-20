```objective-c
// Importamos las bibliotecas necesarias
#import <Foundation/Foundation.h>

// Creamos una clase llamada "Persona"
@interface Persona : NSObject
{
    NSString *nombre;
    int edad;
}

// Declaramos las propiedades de la clase
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) int edad;

// Implementamos los métodos de la clase
- (id)initWithNombre:(NSString *)nombre edad:(int)edad;
- (void)saludar;

@end

// Implementamos los métodos de la clase
@implementation Persona

- (id)initWithNombre:(NSString *)nombre edad:(int)edad
{
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

- (void)saludar
{
    NSLog(@"Hola, mi nombre es %@ y tengo %d años", self.nombre, self.edad);
}

@end

// Creamos una función que devuelve una lista de personas
NSArray *crearListaDePersonas()
{
    NSMutableArray *lista = [NSMutableArray array];

    Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:20];
    Persona *persona2 = [[Persona alloc] initWithNombre:@"María" edad:25];
    Persona *persona3 = [[Persona alloc] initWithNombre:"Pedro" edad:30];

    [lista addObject:persona1];
    [lista addObject:persona2];
    [lista addObject:persona3];

    return lista;
}

// Función principal
int main(int argc, const char * argv[])
{
    @autoreleasepool {
        // Creamos una lista de personas
        NSArray *lista = crearListaDePersonas();

        // Iteramos sobre la lista y saludamos a cada persona
        for (Persona *persona in lista) {
            [persona saludar];
        }
    }
    return 0;
}
```

Explicación:

Este código crea una clase llamada "Persona" con dos propiedades: "nombre" y "edad". También implementa dos métodos: "initWithNombre:edad:" que inicializa la clase con un nombre y una edad, y "saludar" que imprime un mensaje de saludo con el nombre y la edad de la persona.

A continuación, creamos una función llamada "crearListaDePersonas()" que devuelve una lista de personas. Esta función crea tres personas con nombres y edades diferentes y las añade a una lista mutable.

Finalmente, en la función principal, creamos una lista de personas y la recorremos para saludar a cada persona.

Este código es complejo porque:

* Crea una clase con propiedades y métodos.
* Utiliza una lista mutable para almacenar personas.
* Recorre la lista y llama a un método de cada persona.