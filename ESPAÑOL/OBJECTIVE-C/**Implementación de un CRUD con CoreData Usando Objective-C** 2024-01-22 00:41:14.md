```objective-c
// Importación de bibliotecas necesarias
#import <Foundation/Foundation.h>
#import <CoreData/CoreData.h>

// Definición de la clase "Persona"
@interface Persona : NSManagedObject

// Propiedades de la clase "Persona"
@property (nonatomic, retain) NSString *nombre;
@property (nonatomic, retain) NSDate *fechaNacimiento;
@property (nonatomic, retain) NSNumber *edad;

// Métodos de la clase "Persona"
- (void)calcularEdad;

@end

// Definición de la clase "BaseDeDatos"
@interface BaseDeDatos : NSObject

// Método para crear e inicializar la base de datos
- (void)crearBaseDeDatos;

// Método para guardar los datos en la base de datos
- (void)guardarDatos;

// Método para recuperar los datos de la base de datos
- (NSArray *)recuperarDatos;

@end

// Implementación de la clase "Persona"
@implementation Persona

// Método para calcular la edad de la persona
- (void)calcularEdad {
    // Obtener la fecha actual
    NSDate *fechaActual = [NSDate date];
    
    // Calcular la diferencia entre la fecha actual y la fecha de nacimiento
    NSTimeInterval diferencia = [fechaActual timeIntervalSinceDate:self.fechaNacimiento];
    
    // Convertir la diferencia en años
    self.edad = [NSNumber numberWithInteger:(diferencia / (60 * 60 * 24 * 365))];
}

@end

// Implementación de la clase "BaseDeDatos"
@implementation BaseDeDatos

// Método para crear e inicializar la base de datos
- (void)crearBaseDeDatos {
    // Crear el contexto de la base de datos
    NSManagedObjectContext *contexto = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
    
    // Crear el modelo de la base de datos
    NSManagedObjectModel *modelo = [[NSManagedObjectModel alloc] initWithContentsOfURL:[NSURL fileURLWithPath:[[NSBundle mainBundle] pathForResource:@"ModeloBaseDeDatos" ofType:@"momd"]]];
    
    // Crear el coordinador de la base de datos
    NSPersistentStoreCoordinator *coordinador = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:modelo];
    
    // Crear la base de datos
    NSError *error = nil;
    [coordinador addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:[NSURL fileURLWithPath:[[NSBundle mainBundle] pathForResource:@"BaseDeDatos" ofType:@"sqlite"]] options:nil error:&error];
    
    // Establecer el contexto de la base de datos
    contexto.persistentStoreCoordinator = coordinador;
    
    // Guardar el contexto de la base de datos
    [contexto save:&error];
    
    // Liberar los objetos creados
    [contexto release];
    [modelo release];
    [coordinador release];
}

// Método para guardar los datos en la base de datos
- (void)guardarDatos {
    // Obtener el contexto de la base de datos
    NSManagedObjectContext *contexto = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
    
    // Crear una nueva entidad "Persona"
    Persona *persona = [NSEntityDescription insertNewObjectForEntityForName:@"Persona" inManagedObjectContext:contexto];
    
    // Establecer los valores de las propiedades de la entidad "Persona"
    persona.nombre = @"Juan Pérez";
    persona.fechaNacimiento = [NSDate dateWithTimeIntervalSince1970:0];
    [persona calcularEdad];
    
    // Guardar los datos en la base de datos
    NSError *error = nil;
    [contexto save:&error];
    
    // Liberar los objetos creados
    [contexto release];
}

// Método para recuperar los datos de la base de datos
- (NSArray *)recuperarDatos {
    // Obtener el contexto de la base de datos
    NSManagedObjectContext *contexto = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
    
    // Crear una consulta para recuperar los datos de la entidad "Persona"
    NSFetchRequest *consulta = [[NSFetchRequest alloc] init];
    consulta.entity = [NSEntityDescription entityForName:@"Persona" inManagedObjectContext:contexto];
    
    // Ejecutar la consulta
    NSError *error = nil;
    NSArray *resultado = [contexto executeFetchRequest:consulta error:&error];
    
    // Liberar los objetos creados
    [contexto release];
    [consulta release];
    
    // Devolver el resultado de la consulta
    return resultado;
}

@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear una instancia de la clase "BaseDeDatos"
        BaseDeDatos *baseDeDatos = [[BaseDeDatos alloc] init];
        
        // Crear la base de datos
        [baseDeDatos crearBaseDeDatos];
        
        // Guardar los datos en la base de datos
        [baseDeDatos guardarDatos];
        
        // Recuperar los datos de la base de datos
        NSArray *datos = [baseDeDatos recuperarDatos];
        
        // Imprimir los datos recuperados
        for (Persona *persona in datos) {
            NSLog(@"Nombre: %@, Fecha de nacimiento: %@, Edad: %@", persona.nombre, persona.fechaNacimiento, persona.edad);
        }
        
        // Liberar el objeto creado
        [baseDeDatos release];
    }
    return 0;
}
```

Explicación del código:

1. **Definición de la clase "Persona"**: Esta clase representa la entidad "Persona" en la base de datos. Contiene las propiedades "nombre", "fechaNacimiento" y "edad", así como el método "calcularEdad" para calcular la edad de la persona.

2. **Definición de la clase "BaseDeDatos"**: Esta clase se encarga de crear e inicializar la base de datos, así como de guardar y recuperar los datos de la misma.

3. **Implementación de la clase "Persona"**: La implementación del método "calcularEdad" calcula la edad de la persona en función de su fecha de nacimiento.

4. **Implementación de la clase "BaseDeDatos"**: La implementación del método "crearBaseDeDatos" crea la base de datos y la inicializa con el modelo especificado. El método "guardarDatos" guarda los datos en la base de datos y el método "recuperarDatos" recupera