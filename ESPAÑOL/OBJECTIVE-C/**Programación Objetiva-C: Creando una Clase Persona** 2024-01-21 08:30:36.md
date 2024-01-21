```objective-c
#import <Foundation/Foundation.h>

// Definición de una clase personalizada
@interface Persona : NSObject
{
    NSString *nombre;
    int edad;
}

// Declaración de propiedades
@property (nonatomic, retain) NSString *nombre;
@property (nonatomic, assign) int edad;

// Métodos de inicialización
- (id)initWithNombre:(NSString *)nombre edad:(int)edad;
- (void)dealloc;

// Métodos de acceso a las propiedades
- (NSString *)nombre;
- (int)edad;
- (void)setNombre:(NSString *)nombre;
- (void)setEdad:(int)edad;

// Métodos adicionales
- (NSString *)descripcion;
@end

// Implementación de la clase Persona
@implementation Persona

// Método de inicialización
- (id)initWithNombre:(NSString *)nombre edad:(int)edad
{
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

// Método de desinicialización
- (void)dealloc
{
    [nombre release];
    [super dealloc];
}

// Métodos de acceso a las propiedades
- (NSString *)nombre
{
    return nombre;
}

- (int)edad
{
    return edad;
}

- (void)setNombre:(NSString *)nombre
{
    [nombre retain];
    [nombre release];
}

- (void)setEdad:(int)edad
{
    edad = edad;
}

// Método adicional
- (NSString *)descripcion
{
    return [NSString stringWithFormat:@"Nombre: %@, Edad: %d", nombre, edad];
}
@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creación de un objeto de tipo Persona
        Persona *persona = [[Persona alloc] initWithNombre:@"Juan" edad:25];

        // Uso de los métodos de acceso a las propiedades
        NSLog(@"Nombre: %@", persona.nombre);
        NSLog(@"Edad: %d", persona.edad);

        // Uso del método adicional
        NSLog(@"Descripción: %@", persona.descripcion);

        // Liberación del objeto
        [persona release];
    }
    return 0;
}
```

Explicación del código:

1. **Definición de la clase Persona:**

   - La clase `Persona` se define utilizando la sintaxis `@interface`.
   - La clase tiene dos propiedades: `nombre` y `edad`.
   - La clase tiene tres métodos de inicialización: `initWithNombre:edad:`, `dealloc` y `init`.
   - La clase tiene cuatro métodos de acceso a las propiedades: `nombre`, `edad`, `setNombre:` y `setEdad:`.
   - La clase tiene un método adicional: `descripcion`.

2. **Implementación de la clase Persona:**

   - El método `initWithNombre:edad:` se utiliza para inicializar un objeto de tipo `Persona` con un nombre y una edad.
   - El método `dealloc` se utiliza para desinicializar un objeto de tipo `Persona`.
   - Los métodos `nombre`, `edad`, `setNombre:` y `setEdad:` se utilizan para acceder y modificar las propiedades del objeto.
   - El método `descripcion` se utiliza para obtener una descripción del objeto.

3. **Función principal:**

   - La función `main` es el punto de entrada del programa.
   - En la función `main`, se crea un objeto de tipo `Persona` utilizando el método `initWithNombre:edad:`.
   - Se utilizan los métodos de acceso a las propiedades para obtener y modificar las propiedades del objeto.
   - Se utiliza el método `descripcion` para obtener una descripción del objeto.
   - Se libera el objeto utilizando el método `release`.