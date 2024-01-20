```objective-c
#import <Foundation/Foundation.h>

// Definir una clase
@interface Persona : NSObject
{
    // Atributos privados
    NSString *nombre;
    int edad;
    float altura;
}

// Propiedades públicas
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic) int edad;
@property (nonatomic) float altura;

// Métodos de inicialización
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad altura:(float)altura;

// Métodos de acceso
- (NSString *)getNombre;
- (void)setNombre:(NSString *)nombre;
- (int)getEdad;
- (void)setEdad:(int)edad;
- (float)getAltura;
- (void)setAltura:(float)altura;

// Otros métodos
- (void)comer;
- (void)dormir;
- (void)trabajar;
@end

// Código de implementación de la clase
@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad altura:(float)altura
{
    // Llamar al inicializador de la superclase
    self = [super init];
    if (self)
    {
        // Inicializar los atributos privados
        self.nombre = nombre;
        self.edad = edad;
        self.altura = altura;
    }
    return self;
}

// Métodos de acceso
- (NSString *)getNombre
{
    return self.nombre;
}

- (void)setNombre:(NSString *)nombre
{
    self.nombre = nombre;
}

- (int)getEdad
{
    return self.edad;
}

- (void)setEdad:(int)edad
{
    self.edad = edad;
}

- (float)getAltura
{
    return self.altura;
}

- (void)setAltura:(float)altura
{
    self.altura = altura;
}

// Otros métodos
- (void)comer
{
    NSLog(@"%@ está comiendo.", self.nombre);
}

- (void)dormir
{
    NSLog(@"%@ está durmiendo.", self.nombre);
}

- (void)trabajar
{
    NSLog(@"%@ está trabajando.", self.nombre);
}

@end

// Utilizar la clase Persona
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear un objeto de la clase Persona
        Persona *persona = [[Persona alloc] initWithNombre:@"Juan" edad:20 altura:1.75];

        // Imprimir los datos de la persona
        NSLog(@"Nombre: %@\nEdad: %d\nAltura: %.2f", persona.nombre, persona.edad, persona.altura);

        // Llamar a los métodos de la persona
        [persona comer];
        [persona dormir];
        [persona trabajar];
    }
    return 0;
}
```

Explicación del código:

* Se define la clase `Persona` con atributos privados y propiedades públicas para acceder a los atributos.
* Se implementan los métodos de inicialización, acceso y otros métodos.
* Se crea un objeto de la clase `Persona` y se inicializan sus atributos.
* Se imprimen los datos de la persona.
* Se llaman a los métodos de la persona.