```objective-c
// Importar las bibliotecas necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Definir la clase principal
@interface MiClasePrincipal : NSObject

// Definir las propiedades de la clase
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSArray *lista;

// Definir los métodos de la clase
- (void)saludar;
- (void)imprimirLista;

@end

// Implementar la clase principal
@implementation MiClasePrincipal

// Implementar el método saludar
- (void)saludar {
    NSLog(@"Hola, %@!", self.nombre);
}

// Implementar el método imprimirLista
- (void)imprimirLista {
    for (NSString *elemento in self.lista) {
        NSLog(@"%@", elemento);
    }
}

@end

// Crear una instancia de la clase principal
MiClasePrincipal *instancia = [[MiClasePrincipal alloc] init];

// Asignar valores a las propiedades de la instancia
instancia.nombre = @"Juan";
instancia.lista = @[@"Manzana", @"Naranja", @"Plátano"];

// Llamar a los métodos de la instancia
[instancia saludar];
[instancia imprimirLista];
```

Este código en Objective-C crea una clase llamada MiClasePrincipal que tiene dos propiedades: nombre y lista. También define dos métodos: saludar e imprimirLista.

Luego, se crea una instancia de la clase MiClasePrincipal y se le asignan valores a sus propiedades. Finalmente, se llaman a los métodos de la instancia para saludar al usuario e imprimir la lista de elementos.

Este código es un ejemplo de cómo crear una clase en Objective-C y utilizarla para almacenar y manipular datos.