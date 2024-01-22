```objective-c
// Importaciones necesarias
#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>

// Definición de la clase principal
@interface MiAplicacion : UIViewController

// Propiedades y métodos de la clase

@end

// Implementación de la clase principal
@implementation MiAplicacion

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Configuración inicial de la vista
    self.view.backgroundColor = [UIColor whiteColor];

    // Crear un contexto de CoreData
    NSManagedObjectContext *contexto = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
    
    // Crear una nueva entidad "Persona"
    NSEntityDescription *entidadPersona = [NSEntityDescription entityForName:@"Persona" inManagedObjectContext:contexto];
    
    // Crear una nueva instancia de la entidad "Persona"
    NSManagedObject *persona = [[NSManagedObject alloc] initWithEntity:entidadPersona insertIntoManagedObjectContext:contexto];
    
    // Establecer los valores de las propiedades de la persona
    [persona setValue:@"Juan" forKey:@"nombre"];
    [persona setValue:@"García" forKey:@"apellido"];
    [persona setValue:[NSNumber numberWithInt:30] forKey:@"edad"];
    
    // Guardar los cambios en CoreData
    NSError *error = nil;
    if ([contexto save:&error]) {
        NSLog(@"Persona guardada exitosamente");
    } else {
        NSLog(@"Error al guardar la persona: %@", error);
    }
    
    // Crear una consulta para recuperar todas las personas
    NSFetchRequest *consulta = [[NSFetchRequest alloc] initWithEntityName:@"Persona"];

    // Ejecutar la consulta
    NSArray *personas = [contexto executeFetchRequest:consulta error:&error];
    
    // Iterar sobre las personas recuperadas
    for (NSManagedObject *persona in personas) {
        NSLog(@"Nombre: %@, Apellido: %@, Edad: %@", [persona valueForKey:@"nombre"], [persona valueForKey:@"apellido"], [persona valueForKey:@"edad"]);
    }
}

@end
```

**Explicación del código:**

* El código comienza importando las bibliotecas necesarias, `UIKit` y `CoreData`.
* Se define la clase `MiAplicacion` que extiende a `UIViewController`, esto significa que `MiAplicacion` es un controlador de vistas, que es una clase base para todas las clases que controlan una vista en una aplicación iOS.
* Se añade un método `viewDidLoad` a la clase `MiAplicacion`, este método se llama cuando la vista se carga por primera vez.
* El método `viewDidLoad` configura la vista inicial, establece el color de fondo de la vista a blanco y crea un contexto de `CoreData`.
* El contexto de `CoreData` se utiliza para administrar los objetos persistentes en `CoreData`.
* Se crea una nueva entidad "Persona" en `CoreData`.
* Se crea una nueva instancia de la entidad "Persona" y se establecen los valores de sus propiedades.
* Los cambios se guardan en `CoreData`.
* Se crea una consulta para recuperar todas las personas de `CoreData`.
* La consulta se ejecuta y se itera sobre las personas recuperadas, imprimiendo sus propiedades en la consola.