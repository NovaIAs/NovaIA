```objective-c
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear un diccionario con claves y valores en español
        NSDictionary *diccionario = @{
            @"hola": @"mundo",
            @"gracias": @"por usar Objective-C",
            @"adiós": @"hasta la próxima"
        };

        // Iterar sobre las claves y valores del diccionario
        for (NSString *clave in diccionario) {
            NSString *valor = diccionario[clave];
            NSLog(@"Clave: %@, Valor: %@", clave, valor);
        }

        // Crear un arreglo con elementos en español
        NSArray *arreglo = @[@"uno", @"dos", @"tres", @"cuatro", @"cinco"];

        // Iterar sobre los elementos del arreglo
        for (NSString *elemento in arreglo) {
            NSLog(@"Elemento: %@", elemento);
        }

        // Crear una cadena de texto en español
        NSString *cadena = @"¡Hola, mundo!";

        // Obtener la longitud de la cadena de texto
        NSUInteger longitud = cadena.length;

        // Imprimir la longitud de la cadena de texto
        NSLog(@"Longitud de la cadena: %ld", longitud);

        // Obtener el carácter en la posición 0 de la cadena de texto
        unichar caracter = [cadena characterAtIndex:0];

        // Imprimir el carácter en la posición 0 de la cadena de texto
        NSLog(@"Carácter en la posición 0: %C", caracter);

        // Crear un bloque de código que reciba un número y devuelva su cuadrado
        NSNumber *(^cuadrado)(NSNumber *) = ^(NSNumber *numero) {
            return @(numero.intValue * numero.intValue);
        };

        // Llamar al bloque de código con el número 5
        NSNumber *resultado = cuadrado(@5);

        // Imprimir el resultado
        NSLog(@"Resultado: %@", resultado);
    }

    return 0;
}
```

Este código crea un diccionario, un arreglo, una cadena de texto y un bloque de código en español. Luego, itera sobre el diccionario y el arreglo, obtiene la longitud y el carácter en la posición 0 de la cadena de texto, y llama al bloque de código con el número 5. Finalmente, imprime los resultados.

El código está bien documentado y es fácil de entender. También es eficiente y utiliza las últimas características del lenguaje Objective-C.