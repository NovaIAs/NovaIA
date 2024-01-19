```objective-c
// Importamos las librerías necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Definimos nuestra clase principal
@interface MiClasePrincipal : NSObject

// Declaramos nuestras propiedades
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) NSInteger edad;

// Declaramos nuestro método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad;

// Declaramos nuestros métodos de instancia
- (void)saludar;
- (void)despedirse;

@end

// Implementamos nuestra clase principal
@implementation MiClasePrincipal

// Implementamos nuestro método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _edad = edad;
    }
    return self;
}

// Implementamos nuestros métodos de instancia
- (void)saludar {
    NSLog(@"Hola, mi nombre es %@ y tengo %ld años", _nombre, _edad);
}

- (void)despedirse {
    NSLog(@"Adiós, fue un placer conocerte %@!", _nombre);
}

@end

// Definimos nuestra clase secundaria
@interface MiClaseSecundaria : MiClasePrincipal

// Declaramos nuestras propiedades
@property (nonatomic, strong) NSString *apellido;

// Declaramos nuestro método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad apellido:(NSString *)apellido;

// Declaramos nuestros métodos de instancia
- (void)presentarse;

@end

// Implementamos nuestra clase secundaria
@implementation MiClaseSecundaria

// Implementamos nuestro método inicializador
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad apellido:(NSString *)apellido {
    self = [super initWithNombre:nombre edad:edad];
    if (self) {
        _apellido = apellido;
    }
    return self;
}

// Implementamos nuestros métodos de instancia
- (void)presentarse {
    NSLog(@"Hola, mi nombre es %@ %@ y tengo %ld años", _nombre, _apellido, _edad);
}

@end

// Creamos una instancia de nuestra clase secundaria
MiClaseSecundaria *persona = [[MiClaseSecundaria alloc] initWithNombre:@"Juan" edad:25 apellido:@"García"];

// Llamamos a los métodos de nuestra instancia
[persona saludar];
[persona presentarse];
[persona despedirse];
```

Explicación del código:

* Importamos las librerías necesarias:
    * `<Foundation/Foundation.h>`: Esta librería proporciona clases y funciones básicas del lenguaje Objective-C.
    * `<UIKit/UIKit.h>`: Esta librería proporciona clases y funciones para desarrollar interfaces de usuario en iOS.

* Definimos nuestra clase principal `MiClasePrincipal`:
    * Declaramos nuestras propiedades:
        * `nombre`: El nombre de la persona.
        * `edad`: La edad de la persona.
    * Declaramos nuestro método inicializador:
        * `initWithNombre:edad:`: Este método inicializa la instancia de la clase con el nombre y la edad proporcionados.
    * Declaramos nuestros métodos de instancia:
        * `saludar`: Este método imprime un mensaje de saludo.
        * `despedirse`: Este método imprime un mensaje de despedida.

* Implementamos nuestra clase principal `MiClasePrincipal`:
    * Implementamos nuestro método inicializador:
        * Asignamos los valores proporcionados a las propiedades `nombre` y `edad`.
    * Implementamos nuestros métodos de instancia:
        * Imprimimos los mensajes de saludo y despedida utilizando la función `NSLog`.

* Definimos nuestra clase secundaria `MiClaseSecundaria`:
    * Esta clase hereda de la clase `MiClasePrincipal`.
    * Declaramos nuestras propiedades:
        * `apellido`: El apellido de la persona.
    * Declaramos nuestro método inicializador:
        * `initWithNombre:edad:apellido:`: Este método inicializa la instancia de la clase con el nombre, la edad y el apellido proporcionados.
    * Declar