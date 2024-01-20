```objective-c
// Codificación de caracteres UTF-8

#import <Foundation/Foundation.h>

int main(int argc, char *argv[]) {
    @autoreleasepool {
        // Crear un NSMutableData para almacenar el texto codificado en UTF-8
        NSMutableData *data = [NSMutableData data];

        // Agregar texto a los datos
        NSString *texto = @"¡Hola, mundo!";
        [data appendData:[texto dataUsingEncoding:NSUTF8StringEncoding]];

        // Escribir los datos en un archivo
        NSError *error;
        [data writeToFile:@"archivo.txt" options:NSDataWritingAtomic error:&error];
        if (error) {
            NSLog(@"Error al escribir el archivo: %@", error);
            return 1;
        }

        // Leer los datos del archivo
        NSData *datosLeidos = [NSData dataWithContentsOfFile:@"archivo.txt"];
        if (!datosLeidos) {
            NSLog(@"Error al leer el archivo");
            return 1;
        }

        // Obtener el texto decodificado de los datos
        NSString *textoDecodificado = [[NSString alloc] initWithData:datosLeidos encoding:NSUTF8StringEncoding];

        // Imprimir el texto decodificado
        NSLog(@"%@", textoDecodificado);
    }
    
    return 0;
}
```

Explicación:

1. Importamos la biblioteca Foundation, que es una biblioteca estándar de Objective-C que proporciona clases y métodos fundamentales para el desarrollo de aplicaciones en macOS e iOS.
2. Definimos la función `main`, que es el punto de entrada del programa.
3. Creamos un objeto `NSMutableData` para almacenar el texto codificado en UTF-8.
4. Agregamos el texto "¡Hola, mundo!" al objeto `NSMutableData` utilizando el método `appendData`.
5. Escribimos los datos en un archivo utilizando el método `writeToFile`.
6. Leemos los datos del archivo utilizando el método `dataWithContentsOfFile`.
7. Obtenemos el texto decodificado de los datos utilizando el método `initWithData`.
8. Imprimimos el texto decodificado en la consola utilizando el método `NSLog`.

Este código es más complejo que el anterior porque demuestra algunos conceptos más avanzados de Objective-C, como el uso de objetos `NSMutableData` y `NSData` para almacenar y leer datos de archivos.