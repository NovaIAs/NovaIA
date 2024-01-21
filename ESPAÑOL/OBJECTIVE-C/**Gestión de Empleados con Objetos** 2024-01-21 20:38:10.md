```objective-c
// Definiendo la clase Empleado
@interface Empleado : NSObject

// Propiedades privadas
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSString *apellidos;
@property (nonatomic, assign) int edad;
@property (nonatomic, strong) NSString *puesto;
@property (nonatomic, strong) NSDate *fechaContratacion;

// Métodos de inicialización
- (instancetype)initWithNombre:(NSString *)nombre
                     apellidos:(NSString *)apellidos
                          edad:(int)edad
                         puesto:(NSString *)puesto
             fechaContratacion:(NSDate *)fechaContratacion;

// Otros métodos
- (NSString *)nombreCompleto;
- (int)calcularAntiguedad;

@end

// Implementando la clase Empleado
@implementation Empleado

// Método de inicialización
- (instancetype)initWithNombre:(NSString *)nombre
                     apellidos:(NSString *)apellidos
                          edad:(int)edad
                         puesto:(NSString *)puesto
             fechaContratacion:(NSDate *)fechaContratacion {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _apellidos = apellidos;
        _edad = edad;
        _puesto = puesto;
        _fechaContratacion = fechaContratacion;
    }
    return self;
}

// Otros métodos
- (NSString *)nombreCompleto {
    return [NSString stringWithFormat:@"%@ %@", _nombre, _apellidos];
}

- (int)calcularAntiguedad {
    NSDate *hoy = [NSDate date];
    NSCalendar *calendario = [[NSCalendar alloc] initWithCalendarIdentifier:NSCalendarIdentifierGregorian];
    NSDateComponents *componentes = [calendario components:(NSCalendarUnitYear | NSCalendarUnitMonth | NSCalendarUnitDay) fromDate:_fechaContratacion toDate:hoy options:0];
    return componentes.year;
}

@end

// Definiendo la clase Departamento
@interface Departamento : NSObject

// Propiedades privadas
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSMutableArray *empleados;

// Métodos de inicialización
- (instancetype)initWithNombre:(NSString *)nombre;

// Otros métodos
- (void)agregarEmpleado:(Empleado *)empleado;
- (NSArray *)obtenerEmpleados;
- (int)calcularPresupuestoSalarial;

@end

// Implementando la clase Departamento
@implementation Departamento

// Método de inicialización
- (instancetype)initWithNombre:(NSString *)nombre {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _empleados = [[NSMutableArray alloc] init];
    }
    return self;
}

// Otros métodos
- (void)agregarEmpleado:(Empleado *)empleado {
    [_empleados addObject:empleado];
}

- (NSArray *)obtenerEmpleados {
    return _empleados;
}

- (int)calcularPresupuestoSalarial {
    int presupuestoTotal = 0;
    for (Empleado *empleado in _empleados) {
        presupuestoTotal += empleado.salario;
    }
    return presupuestoTotal;
}

@end

// Definiendo la clase Empresa
@interface Empresa : NSObject

// Propiedades privadas
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSMutableArray *departamentos;

// Métodos de inicialización
- (instancetype)initWithNombre:(NSString *)nombre;

// Otros métodos
- (void)agregarDepartamento:(Departamento *)departamento;
- (NSArray *)obtenerDepartamentos;
- (int)calcularPresupuestoTotal;

@end

// Implementando la clase Empresa
@implementation Empresa

// Método de inicialización
- (instancetype)initWithNombre:(NSString *)nombre {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _departamentos = [[NSMutableArray alloc] init];
    }
    return self;
}

// Otros métodos
- (void)agregarDepartamento:(Departamento *)departamento {
    [_departamentos addObject:departamento];
}

- (NSArray *)obtenerDepartamentos {
    return _departamentos;
}

- (int)calcularPresupuestoTotal {
    int presupuestoTotal = 0;
    for (Departamento *departamento in _departamentos) {
        presupuestoTotal += departamento.calcularPresupuestoSalarial;
    }
    return presupuestoTotal;
}

@end

// Creando una instancia de la clase Empresa
Empresa *empresa = [[Empresa alloc] initWithNombre:@"Acme Corporation"];

// Creando instancias de la clase Departamento
Departamento *departamentoVentas = [[Departamento alloc] initWithNombre:@"Ventas"];
Departamento *departamentoMarketing = [[Departamento alloc] initWithNombre:@"Marketing"];

// Creando instancias de la clase Empleado
Empleado *juan = [[Empleado alloc] initWithNombre:@"Juan" apellidos:@"García" edad:30 puesto:@"Vendedor" fechaContratacion:[NSDate dateWithTimeIntervalSinceNow:-(60 * 60 * 24 * 365)]];
Empleado *ana = [[Empleado alloc] initWithNombre:@"Ana" apellidos:@"López" edad:28 puesto:@"Gerente de ventas" fechaContratacion:[NSDate dateWithTimeIntervalSinceNow:-(60 * 60 * 24 * 365 * 2)]];
Empleado *pedro = [[Empleado alloc] initWithNombre:@"Pedro" apellidos:@"Sánchez" edad:26 puesto:@"Ejecutivo de marketing" fechaContratacion:[NSDate dateWithTimeIntervalSinceNow:-(60 * 60 * 24 * 365 * 3)]];
Empleado *maría = [[Empleado alloc] initWithNombre:@"María" apellidos@"Ruiz" edad:24 puesto:@"Asistente de marketing" fechaContratacion:[NSDate dateWithTimeIntervalSinceNow:-(60 * 60 * 24 * 365)]];

// Agregando empleados a los departamentos
[departamentoVentas agregarEmpleado:juan];
[departamentoVentas agregarEmpleado:ana];
[departamentoMarketing agregarEmpleado:pedro];
[departamentoMarketing agregarEmpleado:maría];

// Agregando departamentos a la empresa
[empresa agregarDepartamento:departamentoVentas];
[empresa agregarDepartamento:departamentoMarketing];

// Imprimiendo información sobre la empresa
NSLog(@"Nombre de la empresa: %@", empresa.nombre);
NSLog(@"Presupuesto total de la empresa: %d", empresa.calcularPresupuestoTotal);

// Imprimiendo información sobre los departamentos
for (Departamento *departamento in empresa.obtenerDepartamentos) {
    NSLog(@"Nombre del departamento: %@", departamento.nombre);
    NSLog(@"Presupuesto salarial del departamento: %d", departamento.calcularPresupuestoSalarial);

    // Imprimiendo información sobre los empleados del departamento
    for (Empleado *empleado in departamento.obtenerEmpleados) {
        NSLog(@"Nombre del empleado: %@", empleado.nombreCompleto);
        NSLog(@"Edad del empleado: %d", empleado.edad);
        NSLog(@"Puesto del empleado: %@", empleado.puesto);
        NSLog(@"Fecha de contratación del empleado: %@", empleado.fechaContratacion);
        NSLog(@"Antigüedad del empleado: %d años", empleado.calcularAntiguedad);
    }
}
```

**Explicación del código:**

* Se definen las clases **Empleado**, **Departamento** y **Empresa**.

* Cada clase tiene sus propias **propiedades** y **métodos**.

* Se crean instancias de las clases **Empleado**, **Departamento** y **Empresa**.

* Se agregan empleados a los departamentos y departamentos a la empresa.

* Se imprimen los datos de la empresa, los departamentos y los empleados.

El código está diseñado para gestionar información sobre una empresa, sus departamentos y sus empleados. Se pueden utilizar distintos mecanismos para gestionar dicha información, como una base de datos, un archivo de texto o una lista en memoria. El código proporcionado utiliza una lista en memoria para almacenar los datos.