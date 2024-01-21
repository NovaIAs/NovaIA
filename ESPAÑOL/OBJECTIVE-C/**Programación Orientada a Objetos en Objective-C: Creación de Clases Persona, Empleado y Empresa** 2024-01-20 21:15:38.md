```objective-c
// Creación de una clase Persona
@interface Persona : NSObject
{
    NSString *nombre;
    int edad;
}

// Propiedades
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) int edad;

// Métodos
- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad;
- (NSString *)nombre;
- (void)setNombre:(NSString *)nombre;
- (int)edad;
- (void)setEdad:(int)edad;
@end

// implementación de la clase Persona
@implementation Persona

// Método inicializador
- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad
{
    if (self = [super init]) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

// Método para obtener el nombre
- (NSString *)nombre
{
    return nombre;
}

// Método para establecer el nombre
- (void)setNombre:(NSString *)nombre
{
    self.nombre = nombre;
}

// Método para obtener la edad
- (int)edad
{
    return edad;
}

// Método para establecer la edad
- (void)setEdad:(int)edad
{
    self.edad = edad;
}
@end

// Creación de una clase Empleado que hereda de Persona
@interface Empleado : Persona
{
    NSString *cargo;
    double salario;
}

// Propiedades
@property (nonatomic, strong) NSString *cargo;
@property (nonatomic, assign) double salario;

// Métodos
- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad andCargo:(NSString *)cargo andSalario:(double)salario;
- (NSString *)cargo;
- (void)setCargo:(NSString *)cargo;
- (double)salario;
- (void)setSalario:(double)salario;
@end

// Implementación de la clase Empleado
@implementation Empleado

// Método inicializador
- (id)initWithNombre:(NSString *)nombre andEdad:(int)edad andCargo:(NSString *)cargo andSalario:(double)salario
{
    if (self = [super initWithNombre:nombre andEdad:edad]) {
        self.cargo = cargo;
        self.salario = salario;
    }
    return self;
}

// Método para obtener el cargo
- (NSString *)cargo
{
    return cargo;
}

// Método para establecer el cargo
- (void)setCargo:(NSString *)cargo
{
    self.cargo = cargo;
}

// Método para obtener el salario
- (double)salario
{
    return salario;
}

// Método para establecer el salario
- (void)setSalario:(double)salario
{
    self.salario = salario;
}
@end

// Creación de una clase Empresa
@interface Empresa : NSObject
{
    NSString *nombre;
    NSMutableArray *empleados;
}

// Propiedades
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSMutableArray *empleados;

// Métodos
- (id)initWithNombre:(NSString *)nombre;
- (NSString *)nombre;
- (void)setNombre:(NSString *)nombre;
- (NSMutableArray *)empleados;
- (void)setEmpleados:(NSMutableArray *)empleados;
- (void)agregarEmpleado:(Empleado *)empleado;
- (void)eliminarEmpleado:(Empleado *)empleado;
- (Empleado *)buscarEmpleadoPorNombre:(NSString *)nombre;
@end

// Implementación de la clase Empresa
@implementation Empresa

// Método inicializador
- (id)initWithNombre:(NSString *)nombre
{
    if (self = [super init]) {
        self.nombre = nombre;
        self.empleados = [[NSMutableArray alloc] init];
    }
    return self;
}

// Método para obtener el nombre
- (NSString *)nombre
{
    return nombre;
}

// Método para establecer el nombre
- (void)setNombre:(NSString *)nombre
{
    self.nombre = nombre;
}

// Método para obtener los empleados
- (NSMutableArray *)empleados
{
    return empleados;
}

// Método para establecer los empleados
- (void)setEmpleados:(NSMutableArray *)empleados
{
    self.empleados = empleados;
}

// Método para agregar un empleado
- (void)agregarEmpleado:(Empleado *)empleado
{
    [self.empleados addObject:empleado];
}

// Método para eliminar un empleado
- (void)eliminarEmpleado:(Empleado *)empleado
{
    [self.empleados removeObject:empleado];
}

// Método para buscar un empleado por nombre
- (Empleado *)buscarEmpleadoPorNombre:(NSString *)nombre
{
    for (Empleado *empleado in self.empleados) {
        if ([empleado.nombre isEqualToString:nombre]) {
            return empleado;
        }
    }
    return nil;
}
@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creación de una empresa
        Empresa *empresa = [[Empresa alloc] initWithNombre:@"Acme Corporation"];

        // Creación de empleados
        Empleado *empleado1 = [[Empleado alloc] initWithNombre:@"Juan" andEdad:30 andCargo:@"Ingeniero" andSalario:50000];
        Empleado *empleado2 = [[Empleado alloc] initWithNombre:@"María" andEdad:25 andCargo:@"Contadora" andSalario:40000];
        Empleado *empleado3 = [[Empleado alloc] initWithNombre:@"Pedro" andEdad:40 andCargo:@"Gerente" andSalario:60000];

        // Agregar empleados a la empresa
        [empresa agregarEmpleado:empleado1];
        [empresa agregarEmpleado:empleado2];
        [empresa agregarEmpleado:empleado3];

        // Buscar un empleado por nombre
        Empleado *empleadoBuscado = [empresa buscarEmpleadoPorNombre:@"María"];

        // Imprimir el nombre y el salario del empleado buscado
        NSLog(@"Nombre: %@, Salario: %.2f", empleadoBuscado.nombre, empleadoBuscado.salario);
    }
    return 0;
}
```

Explicación del código:

* Se crea una clase Persona con propiedades nombre y edad, y métodos para obtener y establecer estos valores.
* Se crea una clase Empleado que hereda de Persona y añade propiedades cargo y salario, y métodos para obtener y establecer estos valores.
* Se crea una clase Empresa que tiene propiedades nombre y empleados, y métodos para obtener, establecer, agregar, eliminar y buscar empleados.
* Se crea una función principal que crea una empresa, agrega empleados a la empresa y busca un empleado por nombre.
* Se imprime el nombre y el salario del empleado buscado.

Este código es un ejemplo de cómo se puede usar la herencia y la composición en Objective-C para crear clases y objetos complejos. También muestra cómo se puede usar un arreglo para almacenar una colección de objetos.