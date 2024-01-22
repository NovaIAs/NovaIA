```objective-c
// Importamos las bibliotecas necesarias.
#import <Foundation/Foundation.h>

// Definimos una clase llamada "Coche" que representa un coche.
@interface Coche : NSObject

// Definimos las propiedades de la clase "Coche".
@property (nonatomic, strong) NSString *marca;
@property (nonatomic, strong) NSString *modelo;
@property (nonatomic, assign) NSInteger anio;

// Definimos el método inicializador de la clase "Coche".
- (instancetype)initWithMarca:(NSString *)marca modelo:(NSString *)modelo anio:(NSInteger)anio;

// Definimos un método para obtener la descripción del coche.
- (NSString *)description;

@end

// Implementamos la clase "Coche".
@implementation Coche

// Implementamos el método inicializador de la clase "Coche".
- (instancetype)initWithMarca:(NSString *)marca modelo:(NSString *)modelo anio:(NSInteger)anio {
  self = [super init];
  if (self) {
    self.marca = marca;
    self.modelo = modelo;
    self.anio = anio;
  }
  return self;
}

// Implementamos el método para obtener la descripción del coche.
- (NSString *)description {
  return [NSString stringWithFormat:@"Marca: %@, Modelo: %@, Año: %ld", self.marca, self.modelo, self.anio];
}

@end

// Definimos una clase llamada "Garaje" que representa un garaje.
@interface Garaje : NSObject

// Definimos las propiedades de la clase "Garaje".
@property (nonatomic, strong) NSMutableArray<Coche *> *coches;

// Definimos el método inicializador de la clase "Garaje".
- (instancetype)init;

// Definimos un método para añadir un coche al garaje.
- (void)addCoche:(Coche *)coche;

// Definimos un método para obtener la lista de coches del garaje.
- (NSArray<Coche *> *)coches;

@end

// Implementamos la clase "Garaje".
@implementation Garaje

// Implementamos el método inicializador de la clase "Garaje".
- (instancetype)init {
  self = [super init];
  if (self) {
    self.coches = [[NSMutableArray alloc] init];
  }
  return self;
}

// Implementamos el método para añadir un coche al garaje.
- (void)addCoche:(Coche *)coche {
  [self.coches addObject:coche];
}

// Implementamos el método para obtener la lista de coches del garaje.
- (NSArray<Coche *> *)coches {
  return self.coches;
}

@end

// Creamos un garaje.
Garaje *garaje = [[Garaje alloc] init];

// Creamos un coche.
Coche *coche1 = [[Coche alloc] initWithMarca:@"Toyota" modelo:@"Camry" anio:2020];

// Añadimos el coche al garaje.
[garaje addCoche:coche1];

// Creamos otro coche.
Coche *coche2 = [[Coche alloc] initWithMarca:@"Honda" modelo:@"Civic" anio:2021];

// Añadimos el coche al garaje.
[garaje addCoche:coche2];

// Obtenemos la lista de coches del garaje.
NSArray<Coche *> *coches = [garaje coches];

// Imprimimos la lista de coches del garaje.
for (Coche *coche in coches) {
  NSLog(@"%@", [coche description]);
}
```

Explicación del código:

* Definimos dos clases, "Coche" y "Garaje".
* La clase "Coche" representa un coche y tiene las propiedades "marca", "modelo" y "anio".
* La clase "Garaje" representa un garaje y tiene la propiedad "coches", que es una lista de coches.
* Creamos un garaje y dos coches.
* Añadimos los coches al garaje.
* Obtenemos la lista de coches del garaje.
* Imprimimos la lista de coches del garaje.