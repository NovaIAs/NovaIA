```objective-c
#import <Foundation/Foundation.h>

// Definir una clase llamada "Coche" con propiedades y métodos
@interface Coche : NSObject

@property (nonatomic, strong) NSString *marca;
@property (nonatomic, strong) NSString *modelo;
@property (nonatomic) int año;
@property (nonatomic) float precio;

// Método para inicializar el objeto
- (instancetype)initWithMarca:(NSString *)marca modelo:(NSString *)modelo año:(int)año precio:(float)precio;

// Método para mostrar la información del coche
- (void)mostrarInformacion;

@end

// Implementación de la clase "Coche"
@implementation Coche

// Método para inicializar el objeto
- (instancetype)initWithMarca:(NSString *)marca modelo:(NSString *)modelo año:(int)año precio:(float)precio {
    self = [super init];
    if (self) {
        self.marca = marca;
        self.modelo = modelo;
        self.año = año;
        self.precio = precio;
    }
    return self;
}

// Método para mostrar la información del coche
- (void)mostrarInformacion {
    NSLog(@"Marca: %@", self.marca);
    NSLog(@"Modelo: %@", self.modelo);
    NSLog(@"Año: %d", self.año);
    NSLog(@"Precio: %.2f", self.precio);
}

@end

// Definir una clase llamada "ListaCoches" para gestionar una lista de coches
@interface ListaCoches : NSObject

@property (nonatomic, strong) NSMutableArray *coches;

// Método para añadir un coche a la lista
- (void)addCoche:(Coche *)coche;

// Método para eliminar un coche de la lista
- (void)removeCoche:(Coche *)coche;

// Método para obtener el número de coches en la lista
- (int)count;

// Método para obtener un coche de la lista por su índice
- (Coche *)getCocheAtIndex:(int)index;

@end

// Implementación de la clase "ListaCoches"
@implementation ListaCoches

// Método para inicializar el objeto
- (instancetype)init {
    self = [super init];
    if (self) {
        self.coches = [[NSMutableArray alloc] init];
    }
    return self;
}

// Método para añadir un coche a la lista
- (void)addCoche:(Coche *)coche {
    [self.coches addObject:coche];
}

// Método para eliminar un coche de la lista
- (void)removeCoche:(Coche *)coche {
    [self.coches removeObject:coche];
}

// Método para obtener el número de coches en la lista
- (int)count {
    return [self.coches count];
}

// Método para obtener un coche de la lista por su índice
- (Coche *)getCocheAtIndex:(int)index {
    return [self.coches objectAtIndex:index];
}

@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear una lista de coches
        ListaCoches *listaCoches = [[ListaCoches alloc] init];

        // Añadir algunos coches a la lista
        Coche *coche1 = [[Coche alloc] initWithMarca:@"Toyota" modelo:@"Camry" año:2023 precio:30000.00];
        Coche *coche2 = [[Coche alloc] initWithMarca:@"Honda" modelo:@"Accord" año:2022 precio:25000.00];
        Coche *coche3 = [[Coche alloc] initWithMarca:@"Ford" modelo:@"Mustang" año:2021 precio:40000.00];

        [listaCoches addCoche:coche1];
        [listaCoches addCoche:coche2];
        [listaCoches addCoche:coche3];

        // Mostrar la información de los coches en la lista
        for (int i = 0; i < [listaCoches count]; i++) {
            Coche *coche = [listaCoches get