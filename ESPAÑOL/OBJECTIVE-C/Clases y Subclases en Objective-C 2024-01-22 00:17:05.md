```objective-c
// Importar las bibliotecas necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Definir una clase personalizada
@interface MyClass : NSObject

// Declarar las propiedades de la clase
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSArray *lista;
@property (nonatomic, assign) BOOL activo;

// Definir los métodos de la clase
- (instancetype)initWithNombre:(NSString *)nombre lista:(NSArray *)lista activo:(BOOL)activo;
- (void)hacerAlgo;

@end

// Implementar la clase personalizada
@implementation MyClass

// Implementar el método constructor
- (instancetype)initWithNombre:(NSString *)nombre lista:(NSArray *)lista activo:(BOOL)activo {
  self = [super init];
  if (self) {
    _nombre = nombre;
    _lista = lista;
    _activo = activo;
  }
  return self;
}

// Implementar el método hacerAlgo
- (void)hacerAlgo {
  NSLog(@"Haciendo algo...");
}

@end

// Definir una clase que herede de MyClass
@interface MySubclass : MyClass

// Declarar las propiedades de la subclase
@property (nonatomic, assign) NSInteger edad;

// Definir los métodos de la subclase
- (instancetype)initWithNombre:(NSString *)nombre lista:(NSArray *)lista activo:(BOOL)activo edad:(NSInteger)edad;

@end

// Implementar la clase que herede de MyClass
@implementation MySubclass

// Implementar el método constructor
- (instancetype)initWithNombre:(NSString *)nombre lista:(NSArray *)lista activo:(BOOL)activo edad:(NSInteger)edad {
  self = [super initWithNombre:nombre lista:lista activo:activo];
  if (self) {
    _edad = edad;
  }
  return self;
}

@end

// Definir una clase que use la clase personalizada y la subclase
@interface MyOtherClass : NSObject

// Declarar las propiedades de la clase
@property (nonatomic, strong) MyClass *myObject;
@property (nonatomic, strong) MySubclass *mySubclassObject;

// Definir los métodos de la clase
- (instancetype)initWithMyObject:(MyClass *)myObject mySubclassObject:(MySubclass *)mySubclassObject;
- (void)hacerAlgo;

@end

// Implementar la clase que use la clase personalizada y la subclase
@implementation MyOtherClass

// Implementar el método constructor
- (instancetype)initWithMyObject:(MyClass *)myObject mySubclassObject:(MySubclass *)mySubclassObject {
  self = [super init];
  if (self) {
    _myObject = myObject;
    _mySubclassObject = mySubclassObject;
  }
  return self;
}

// Implementar el método hacerAlgo
- (void)hacerAlgo {
  NSLog(@"Haciendo algo con la clase personalizada y la subclase...");
}

@end

// Función principal
int main(int argc, char *argv[]) {
  // Crear una instancia de MyClass
  MyClass *myObject = [[MyClass alloc] initWithNombre:@"Juan" lista:@[@"a", @"b", @"c"] activo:YES];

  // Crear una instancia de MySubclass
  MySubclass *mySubclassObject = [[MySubclass alloc] initWithNombre:@"María" lista:@[@"1", @"2", @"3"] activo:NO edad:25];

  // Crear una instancia de MyOtherClass
  MyOtherClass *myOtherObject = [[MyOtherClass alloc] initWithMyObject:myObject mySubclassObject:mySubclassObject];

  // Hacer algo con la clase personalizada y la subclase
  [myOtherObject hacerAlgo];

  return 0;
}
```

Este código es un ejemplo de cómo crear clases y subclases en Objective-C. La clase `MyClass` define dos propiedades, `nombre` y `lista`, y un método, `hacerAlgo`. La clase `MySubclass` hereda de `MyClass` y define una nueva propiedad, `edad`. La clase `MyOtherClass` utiliza la clase personalizada `MyClass` y la subclase `MySubclass`.

El código también muestra cómo crear instancias de estas clases y cómo utilizarlas. Por último, el código define una función principal, `main`, que crea instancias de las clases y las utiliza para hacer algo.