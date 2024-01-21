```objective-c
// Clase: Vehículo Eléctrico
@interface VehiculoElectrico : NSObject

@property (nonatomic, strong) NSString *nombre; // Nombre del vehículo
@property (nonatomic, strong) NSUInteger autonomia; // Autonomía en kilómetros
@property (nonatomic, strong) NSUInteger potencia; // Potencia en caballos de fuerza
@property (nonatomic, strong) NSUInteger capacidadBateria; // Capacidad de la batería en kWh

@end

@implementation VehiculoElectrico

// Constructor personalizado
- (instancetype)initWithNombre:(NSString *)nombre
                       autonomia:(NSUInteger)autonomia
                        potencia:(NSUInteger)potencia
                 capacidadBateria:(NSUInteger)capacidadBateria {
  self = [super init];
  if (self) {
    self.nombre = nombre;
    self.autonomia = autonomia;
    self.potencia = potencia;
    self.capacidadBateria = capacidadBateria;
  }
  return self;
}

@end

// Clase: Estación de Recarga
@interface EstacionDeRecarga : NSObject

@property (nonatomic, strong) NSString *nombre; // Nombre de la estación
@property (nonatomic, strong) NSUInteger potencia; // Potencia máxima de recarga
@property (nonatomic, strong) NSUInteger numeroPuestos; // Número de puestos de recarga

@end

@implementation EstacionDeRecarga

// Constructor personalizado
- (instancetype)initWithNombre:(NSString *)nombre
                       potencia:(NSUInteger)potencia
                numeroPuestos:(NSUInteger)numeroPuestos {
  self = [super init];
  if (self) {
    self.nombre = nombre;
    self.potencia = potencia;
    self.numeroPuestos = numeroPuestos;
  }
  return self;
}

@end

// Clase principal
int main(int argc, const char *argv[]) {
  @autoreleasepool {
    // Crear objetos de Vehículo Eléctrico
    VehiculoElectrico *tesla = [[VehiculoElectrico alloc] initWithNombre:@"Tesla Model S"
                                                              autonomia:625
                                                               potencia:1020
                                                        capacidadBateria:100];
    VehiculoElectrico *bmw = [[VehiculoElectrico alloc] initWithNombre:@"BMW i8"
                                                           autonomia:530
                                                            potencia:750
                                                     capacidadBateria:70];

    // Crear objetos de Estación de Recarga
    EstacionDeRecarga *estacionA = [[EstacionDeRecarga alloc] initWithNombre:@"Estación A"
                                                                    potencia:150
                                                              numeroPuestos:5];
    EstacionDeRecarga *estacionB = [[EstacionDeRecarga alloc] initWithNombre:@"Estación B"
                                                                    potencia:300
                                                              numeroPuestos:10];

    // Simular la recarga de los vehículos
    tesla.capacidadBateria += [estacionA potencia] / 100;
    bmw.capacidadBateria += [estacionB potencia] / 100;

    // Mostrar los resultados
    NSLog(@"Tesla Model S: Autonomía restante: %lu km, Capacidad de batería: %lu%%", tesla.autonomia, tesla.capacidadBateria);
    NSLog(@"BMW i8: Autonomía restante: %lu km, Capacidad de batería: %lu%%", bmw.autonomia, bmw.capacidadBateria);
  }
  return 0;
}
```

**Explicación:**

* **Clase Vehículo Eléctrico:** Representa un tipo de vehículo alimentado por electricidad. Tiene atributos como el nombre, la autonomía, la potencia y la capacidad de la batería.
* **Clase Estación de Recarga:** Representa un lugar donde los vehículos eléctricos pueden recargar sus baterías. Tiene atributos como el nombre, la potencia máxima de recarga y el número de puestos de recarga.
* **Función main:** Es la función de entrada del programa. Crea objetos de Vehículo Eléctrico y de Estación de Recarga, simula la recarga de los vehículos y muestra los resultados.

Este código es una simulación simple de la recarga de vehículos eléctricos en estaciones de recarga. Es un ejemplo de código más complejo en Objective-C que utiliza múltiples clases y objetos.