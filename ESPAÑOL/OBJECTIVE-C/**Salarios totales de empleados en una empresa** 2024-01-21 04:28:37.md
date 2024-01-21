```objective-c
#import <Foundation/Foundation.h>

// Definir una clase para representar a un empleado
@interface Empleado : NSObject

    // Propiedades
    @property NSString *nombre;
    @property int salario;
    @property NSString *puesto;

    // Métodos
    -(id)initWithNombre:(NSString*)nombre salario:(int)salario puesto:(NSString*)puesto;

@end

// Implementar la clase Empleado
@implementation Empleado

    // Inicializador
    -(id)initWithNombre:(NSString*)nombre salario:(int)salario puesto:(NSString*)puesto {
        if (self = [super init]) {
            _nombre = nombre;
            _salario = salario;
            _puesto = puesto;
        }
        return self;
    }

@end

// Definir una clase para representar una empresa
@interface Empresa : NSObject

    // Propiedades
    @property NSString *nombre;
    @property NSArray<Empleado*> *empleados;

    // Métodos
    -(id)initWithNombre:(NSString*)nombre empleados:(NSArray<Empleado*>*)empleados;

@end

// Implementar la clase Empresa
@implementation Empresa

    // Inicializador
    -(id)initWithNombre:(NSString*)nombre empleados:(NSArray<Empleado*>*)empleados {
        if (self = [super init]) {
            _nombre = nombre;
            _empleados = empleados;
        }
        return self;
    }

    // Obtener el total de salarios de los empleados de la empresa
    -(int)totalSalarios {
        int total = 0;
        for (Empleado *empleado in _empleados) {
            total += empleado.salario;
        }
        return total;
    }

@end

// Crear una empresa
Empresa *empresa = [[Empresa alloc] initWithNombre:@"Acme Inc." empleados:@[
    [[Empleado alloc] initWithNombre:@"Juan Pérez" salario:100000 puesto:@"Gerente"],
    [[Empleado alloc] initWithNombre:@"María García" salario:80000 puesto:@"Ingeniera"],
    [[Empleado alloc] initWithNombre:@"Pedro López" salario:60000 puesto:@"Contador"]
]];

// Obtener el total de salarios de los empleados de la empresa
int totalSalarios = [empresa totalSalarios];

// Imprimir el total de salarios en la consola
NSLog(@"El total de salarios en Acme Inc. es: %d", totalSalarios);
```

Este código define dos clases, `Empleado` y `Empresa`, y luego crea una empresa con tres empleados y obtiene el total de salarios de los empleados de la empresa.

La clase `Empleado` tiene tres propiedades: `nombre`, `salario` y `puesto`. La clase `Empresa` tiene dos propiedades: `nombre` y `empleados`, que es un arreglo de objetos de tipo `Empleado`.

El inicializador de la clase `Empleado` toma como parámetros el nombre, el salario y el puesto del empleado y crea un nuevo objeto de tipo `Empleado` con estos valores.

El inicializador de la clase `Empresa` toma como parámetros el nombre de la empresa y un arreglo de objetos de tipo `Empleado` y crea un nuevo objeto de tipo `Empresa` con estos valores.

El método `totalSalarios` de la clase `Empresa` recorre el arreglo de empleados de la empresa y suma los salarios de cada empleado.

El código crea una empresa llamada "Acme Inc." con tres empleados: Juan Pérez, María García y Pedro López. Luego, llama al método `totalSalarios` de la empresa para obtener el total de salarios de los empleados de la empresa, que es 240000. Finalmente, imprime el total de salarios en la consola.