```objective-c
// Importamos las bibliotecas necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Creamos una clase llamada "MiClase"
@interface MiClase : NSObject

// Declaramos las propiedades de la clase
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSArray *lista;

// Creamos los métodos de la clase
- (void)imprimirNombre;
- (void)imprimirLista;

@end

// Implementamos la clase "MiClase"
@implementation MiClase

// Definimos el método "imprimirNombre"
- (void)imprimirNombre {
    NSLog(@"El nombre de la clase es: %@", self.nombre);
}

// Definimos el método "imprimirLista"
- (void)imprimirLista {
    for (NSString *elemento in self.lista) {
        NSLog(@"Elemento de la lista: %@", elemento);
    }
}

@end

// Creamos una instancia de la clase "MiClase"
MiClase *miClase = [[MiClase alloc] init];

// Asignamos valores a las propiedades de la instancia
miClase.nombre = @"MiClase";
miClase.lista = @[@"Elemento 1", @"Elemento 2", @"Elemento 3"];

// Llamamos a los métodos de la instancia
[miClase imprimirNombre];
[miClase imprimirLista];
```

Explicación del código:

* Importamos las bibliotecas necesarias para trabajar con Objective-C y UIKit.
* Creamos una clase llamada "MiClase" que hereda de la clase NSObject.
* Declaramos las propiedades de la clase, que son dos: nombre y lista.
* Creamos los métodos de la clase, que son dos: imprimirNombre e imprimirLista.
* Implementamos la clase "MiClase" definiendo los métodos de la clase.
* Creamos una instancia de la clase "MiClase" llamada miClase.
* Asignamos valores a las propiedades de la instancia.
* Llamamos a los métodos de la instancia.

Este código crea una clase con dos propiedades y dos métodos. La clase se instancia y se llaman a los métodos, que imprimen el nombre de la clase y la lista de elementos.