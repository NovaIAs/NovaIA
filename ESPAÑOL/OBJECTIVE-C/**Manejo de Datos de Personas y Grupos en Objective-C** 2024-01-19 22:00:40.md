```objective-c
#import <Foundation/Foundation.h>

// Definir una clase "Persona"
@interface Persona : NSObject {
    NSString *nombre;
    int edad;
}

// Métodos de la clase "Persona"
- (void)inicializarConNombre:(NSString *)nombre yEdad:(int)edad;
- (NSString *)obtenerNombre;
- (int)obtenerEdad;

@end

// Implementación de la clase "Persona"
@implementation Persona

// Método para inicializar una instancia de la clase "Persona"
- (void)inicializarConNombre:(NSString *)nombre yEdad:(int)edad {
    // Asignar valores a las propiedades de la instancia
    self.nombre = nombre;
    self.edad = edad;
}

// Método para obtener el nombre de la instancia
- (NSString *)obtenerNombre {
    // Retornar el valor de la propiedad "nombre"
    return self.nombre;
}

// Método para obtener la edad de la instancia
- (int)obtenerEdad {
    // Retornar el valor de la propiedad "edad"
    return self.edad;
}

@end

// Definir una clase "GrupoPersonas"
@interface GrupoPersonas : NSObject {
    NSMutableArray *personas;
}

// Métodos de la clase "GrupoPersonas"
- (void)agregarPersona:(Persona *)persona;
- (NSMutableArray *)obtenerPersonas;

@end

// Implementación de la clase "GrupoPersonas"
@implementation GrupoPersonas

// Método para agregar una persona al grupo
- (void)agregarPersona:(Persona *)persona {
    // Agregar la persona al arreglo "personas"
    [personas addObject:persona];
}

// Método para obtener el arreglo de personas
- (NSMutableArray *)obtenerPersonas {
    // Retornar el arreglo de personas
    return personas;
}

@end

// Crear una instancia de la clase "GrupoPersonas"
GrupoPersonas *grupoPersonas = [[GrupoPersonas alloc] init];

// Crear una instancia de la clase "Persona"
Persona *persona1 = [[Persona alloc] init];

// Inicializar la instancia "persona1" con un nombre y edad
[persona1 inicializarConNombre:@"Juan" yEdad:25];

// Agregar la instancia "persona1" al grupo de personas
[grupoPersonas agregarPersona:persona1];

// Obtener el nombre de la instancia "persona1"
NSString *nombrePersona1 = [persona1 obtenerNombre];

// Obtener la edad de la instancia "persona1"
int edadPersona1 = [persona1 obtenerEdad];

// Obtener el arreglo de personas del grupo
NSMutableArray *personas = [grupoPersonas obtenerPersonas];

// Recorrer el arreglo de personas
for (Persona *persona in personas) {
    // Obtener el nombre de la persona actual
    NSString *nombrePersona = [persona obtenerNombre];

    // Obtener la edad de la persona actual
    int edadPersona = [persona obtenerEdad];

    // Imprimir el nombre y la edad de la persona actual
    NSLog(@"Nombre: %@, Edad: %d", nombrePersona, edadPersona);
}
```

Explicación del código:

1. Definimos dos clases: `Persona` y `GrupoPersonas`. La clase `Persona` representa a una persona con propiedades como nombre y edad, mientras que la clase `GrupoPersonas` representa a un grupo de personas.

2. En la clase `Persona`, definimos métodos para inicializar una instancia de la clase, obtener el nombre de la instancia y obtener la edad de la instancia.

3. En la clase `GrupoPersonas`, definimos métodos para agregar una persona al grupo y obtener el arreglo de personas del grupo.

4. Creamos una instancia de la clase `GrupoPersonas` llamada `grupoPersonas`.

5. Creamos una instancia de la clase `Persona` llamada `persona1` e inicializamos sus propiedades con un nombre y una edad.

6. Agregamos la instancia `persona1` al grupo de personas llamando al método `agregarPersona` en la instancia `grupoPersonas`.

7. Obtenemos el nombre de la instancia `persona1` llamando al método `obtenerNombre` en la instancia `persona1`.

8. Obtenemos la edad de la instancia `persona1` llamando al método `obtenerEdad` en la instancia `persona1`.

9. Obtenemos el arreglo de personas del grupo llamando al método `obtenerPersonas` en la instancia `grupoPersonas`.

10. Recorremos el arreglo de personas e imprimimos el nombre y la edad de cada persona.