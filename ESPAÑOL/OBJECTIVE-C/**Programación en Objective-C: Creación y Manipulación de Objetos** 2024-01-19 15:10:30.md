```objective-c
#import <Foundation/Foundation.h>

// Define una clase llamada "Coche" con propiedades y métodos
@interface Coche : NSObject

@property (nonatomic, strong) NSString *marca;
@property (nonatomic, strong) NSString *modelo;
@property (nonatomic, assign) int año;

- (void)encender;
- (void)apagar;
- (void)acelerar;
- (void)frenar;

@end

// Implementa la clase "Coche"
@implementation Coche

- (void)encender {
    NSLog(@"El coche %@ %@ del año %d ha sido encendido.", self.marca, self.modelo, self.año);
}

- (void)apagar {
    NSLog(@"El coche %@ %@ del año %d ha sido apagado.", self.marca, self.modelo, self.año);
}

- (void)acelerar {
    NSLog(@"El coche %@ %@ del año %d está acelerando.", self.marca, self.modelo, self.año);
}

- (void)frenar {
    NSLog(@"El coche %@ %@ del año %d está frenando.", self.marca, self.modelo, self.año);
}

@end

// Define una clase llamada "Persona" con propiedades y métodos
@interface Persona : NSObject

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSString *apellido;
@property (nonatomic, assign) int edad;

- (void)hablar;
- (void)caminar;
- (void)comer;

@end

// Implementa la clase "Persona"
@implementation Persona

- (void)hablar {
    NSLog(@"La persona %@ %@ de %d años está hablando.", self.nombre, self.apellido, self.edad);
}

- (void)caminar {
    NSLog(@"La persona %@ %@ de %d años está caminando.", self.nombre, self.apellido, self.edad);
}

- (void)comer {
    NSLog(@"La persona %@ %@ de %d años está comiendo.", self.nombre, self.apellido, self.edad);
}

@end

// Crea una instancia de la clase "Coche"
Coche *coche = [[Coche alloc] init];

// Establece las propiedades del coche
coche.marca = @"Toyota";
coche.modelo = @"Yaris";
coche.año = 2020;

// Enciende el coche
[coche encender];

// Acelera el coche
[coche acelerar];

// Frena el coche
[coche frenar];

// Apaga el coche
[coche apagar];

// Crea una instancia de la clase "Persona"
Persona *persona = [[Persona alloc] init];

// Establece las propiedades de la persona
persona.nombre = @"Juan";
persona.apellido = @"García";
persona.edad = 30;

// Hace que la persona hable
[persona hablar];

// Hace que la persona camine
[persona caminar];

// Hace que la persona coma
[persona comer];
```

Explicación del código:

* Primero, se importan las bibliotecas necesarias para utilizar Objective-C.
* Luego, se definen dos clases: "Coche" y "Persona".
* La clase "Coche" tiene propiedades para almacenar la marca, el modelo y el año del coche. También tiene métodos para encender, apagar, acelerar y frenar el coche.
* La clase "Persona" tiene propiedades para almacenar el nombre, el apellido y la edad de la persona. También tiene métodos para hacer que la persona hable, camine y coma.
* Después, se crean instancias de las clases "Coche" y "Persona" y se establecen sus propiedades.
* Por último, se llaman a los métodos de la clase "Coche" para encender, acelerar, frenar y apagar el coche. También se llaman a los métodos de la clase "Persona" para hacer que la persona hable, camine y coma.

El código anterior es un ejemplo de cómo usar Objective-C para crear y manipular objetos.