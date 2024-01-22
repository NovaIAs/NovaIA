# **Creación de una Clase Personalizada**

```objective-c
// Definición de la clase Persona
@interface Persona : NSObject

// Propiedades de la clase Persona
@property NSString *nombre;
@property int edad;

// Método inicializador de la clase Persona
- (instancetype)initWithNombre:(NSString *)nombre yEdad:(int)edad;

// Método para obtener el nombre de la persona
- (NSString *)getNombre;

// Método para obtener la edad de la persona
- (int)getEdad;

@end

// Implementación de la clase Persona
@implementation Persona

// Implementación del método inicializador
- (instancetype)initWithNombre:(NSString *)nombre yEdad:(int)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

// Implementación del método para obtener el nombre
- (NSString *)getNombre {
    return self.nombre;
}

// Implementación del método para obtener la edad
- (int)getEdad {
    return self.edad;
}

@end

# **Uso de la Clase Personalizada**

// Creación de una instancia de la clase Persona
Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" yEdad:25];

// Obtención del nombre y la edad de la persona
NSString *nombre = [persona1 getNombre];
int edad = [persona1 getEdad];

// Impresión del nombre y la edad de la persona
NSLog(@"Nombre: %@, Edad: %d", nombre, edad);

# **Explicación del Código**

**Creación de la Clase Personalizada:**

1. Se define una clase llamada `Persona` que hereda de la clase base `NSObject`.


2. Dentro de la clase `Persona`, se declaran las propiedades `@property` que representan las características de una persona, en este caso: `nombre` y `edad`.


3. Se define el método inicializador `- (instancetype)initWithNombre:(NSString *)nombre yEdad:(int)edad;` que se utiliza para crear una nueva instancia de la clase `Persona` con un nombre y una edad especificados.


4. Se definen los métodos `- (NSString *)getNombre;` y `- (int)getEdad;` que se utilizan para obtener el nombre y la edad de una instancia de la clase `Persona`.


**Uso de la Clase Personalizada:**

1. Se crea una instancia de la clase `Persona` llamada `persona1` utilizando el método inicializador `initWithNombre` y pasando los valores "Juan" y 25 para el nombre y la edad, respectivamente.


2. Se obtienen el nombre y la edad de la persona utilizando los métodos `getNombre` y `getEdad`.


3. Se utiliza el método `NSLog` para imprimir el nombre y la edad de la persona en la consola.