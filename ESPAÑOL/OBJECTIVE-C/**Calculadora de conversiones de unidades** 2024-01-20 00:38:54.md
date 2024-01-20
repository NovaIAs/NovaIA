**Objetivo:** Crear una calculadora de conversiones entre unidades de medida.

**Código:**

```objective-c
#import <Foundation/Foundation.h>

@interface UnidadDeMedida : NSObject

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSNumber *factor;

- (instancetype)initWithNombre:(NSString *)nombre factor:(NSNumber *)factor;

@end

@implementation UnidadDeMedida

- (instancetype)initWithNombre:(NSString *)nombre factor:(NSNumber *)factor {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.factor = factor;
    }
    return self;
}

@end

@interface CalculadoraDeConversiones : NSObject

@property (nonatomic, strong) NSArray *unidadesDeMedida;

- (instancetype)init;

- (NSNumber *)convertirValor:(NSNumber *)valor desdeUnidad:(UnidadDeMedida *)desdeUnidad aUnidad:(UnidadDeMedida *)aUnidad;

@end

@implementation CalculadoraDeConversiones

- (instancetype)init {
    self = [super init];
    if (self) {
        self.unidadesDeMedida = @[
            [[UnidadDeMedida alloc] initWithNombre:@"metros" factor:@1],
            [[UnidadDeMedida alloc] initWithNombre:@"kilómetros" factor:@1000],
            [[UnidadDeMedida alloc] initWithNombre:@"centímetros" factor:@0.01],
            [[UnidadDeMedida alloc] initWithNombre:@"pulgadas" factor:@0.0254],
            [[UnidadDeMedida alloc] initWithNombre:@"pies" factor:@0.3048],
            [[UnidadDeMedida alloc] initWithNombre:@"yardas" factor:@0.9144],
            [[UnidadDeMedida alloc] initWithNombre:@"millas" factor:@1609.34],
        ];
    }
    return self;
}

- (NSNumber *)convertirValor:(NSNumber *)valor desdeUnidad:(UnidadDeMedida *)desdeUnidad aUnidad:(UnidadDeMedida *)aUnidad {
    NSNumber *resultado = [NSNumber numberWithDouble:[valor doubleValue] * [desdeUnidad.factor doubleValue] / [aUnidad.factor doubleValue]];
    return resultado;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        CalculadoraDeConversiones *calculadora = [[CalculadoraDeConversiones alloc] init];

        NSNumber *valor = @100;
        UnidadDeMedida *desdeUnidad = [[UnidadDeMedida alloc] initWithNombre:@"metros" factor:@1];
        UnidadDeMedida *aUnidad = [[UnidadDeMedida alloc] initWithNombre:@"kilómetros" factor:@1000];

        NSNumber *resultado = [calculadora convertirValor:valor desdeUnidad:desdeUnidad aUnidad:aUnidad];

        NSLog(@"%@ de %@ a %@ es %@", resultado, desdeUnidad.nombre, aUnidad.nombre, resultado);
    }
    return 0;
}
```

**Explicación:**

1. Creamos dos clases: `UnidadDeMedida` y `CalculadoraDeConversiones`. `UnidadDeMedida` representa una unidad de medida y tiene un nombre y un factor de conversión. `CalculadoraDeConversiones` es la clase que realizará las conversiones entre unidades de medida.

2. En la función `main()` creamos una instancia de `CalculadoraDeConversiones`, un valor, una unidad de medida desde la que se realizará la conversión y una unidad de medida a la que se convertirá el valor.

3. Llamamos al método `convertirValor()` de la calculadora, pasando el valor, la unidad de medida desde la que se realizará la conversión y la unidad de medida a la que se convertirá el valor.

4. El método `convertirValor()` calcula el valor convertido multiplicando el valor original por el factor de conversión de la unidad de medida desde la que se realizará la conversión y dividiéndolo por el factor de conversión de la unidad de medida a la que se convertirá el valor.

5. Imprimimos el resultado en la consola usando `NSLog()`.