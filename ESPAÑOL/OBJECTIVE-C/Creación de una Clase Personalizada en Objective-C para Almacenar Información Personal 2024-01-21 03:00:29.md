```objective-c
#import <Foundation/Foundation.h>

// Definir una interfaz para nuestra clase personalizada
@interface MiClasePersonalizada : NSObject

// Propiedades de la clase
@property (strong, nonatomic) NSString *nombre;
@property (strong, nonatomic) NSDate *fechaNacimiento;

// Métodos de la clase
- (instancetype)initWithNombre:(NSString *)nombre fechaNacimiento:(NSDate *)fechaNacimiento;
- (NSString *)descripcion;

@end

// Implementar la clase
@implementation MiClasePersonalizada

// Método inicializador
- (instancetype)initWithNombre:(NSString *)nombre fechaNacimiento:(NSDate *)fechaNacimiento {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _fechaNacimiento = fechaNacimiento;
    }
    return self;
}

// Método para obtener una descripción de la clase
- (NSString *)descripcion {
    NSDateFormatter *formateador = [[NSDateFormatter alloc] init];
    [formateador setDateFormat:@"dd/MM/yyyy"];
    NSString *fechaNacimientoFormateada = [formateador stringFromDate:_fechaNacimiento];
    
    return [NSString stringWithFormat:@"Nombre: %@, Fecha de Nacimiento: %@", _nombre, fechaNacimientoFormateada];
}

@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Crear una instancia de nuestra clase personalizada
        MiClasePersonalizada *persona1 = [[MiClasePersonalizada alloc] initWithNombre:@"Juan" fechaNacimiento:[NSDate dateWithTimeIntervalSince1970:0]];
        
        // Imprimir la descripción de la clase
        NSLog(@"%@", [persona1 descripcion]);
        
    }
    return 0;
}
```

Explicación del código:

1. Importamos la librería Foundation, que contiene las clases y funciones básicas del lenguaje Objective-C.

2. Definimos la interfaz de nuestra clase personalizada `MiClasePersonalizada`. Esta interfaz incluye propiedades y métodos para nuestra clase.

3. Implementamos la clase `MiClasePersonalizada`. La implementación incluye el método inicializador y el método para obtener una descripción de la clase.

4. En la función `main`, creamos una instancia de nuestra clase personalizada y llamamos al método `descripcion` para imprimir la información de la instancia.

Este código crea una clase personalizada con propiedades y métodos, y la utiliza para almacenar y mostrar información sobre una persona.