```objective-c
#import <Foundation/Foundation.h>

@interface MyClass : NSObject

// Propiedades
@property NSString *nombre;
@property int edad;

// Métodos
- (void)saludar;

@end

@implementation MyClass

// Implementación del método saludar
- (void)saludar {
    NSLog(@"Hola, mi nombre es %@ y tengo %d años.", self.nombre, self.edad);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear una instancia de MyClass
        MyClass *miClase = [[MyClass alloc] init];
        
        // Establecer las propiedades de la instancia
        miClase.nombre = @"Juan";
        miClase.edad = 30;
        
        // Llamar al método saludar de la instancia
        [miClase saludar];
    }
    return 0;
}
```

Explicación del código:

1. Importación de la biblioteca `Foundation`: Esta línea importa la biblioteca Foundation, que proporciona funcionalidades básicas para el desarrollo de aplicaciones en Objective-C.

2. Definición de la clase `MyClass`: Esta parte del código define una nueva clase llamada `MyClass`, que es un objeto contenedor que puede almacenar propiedades y métodos.

3. Propiedades de la clase: Dentro de la definición de la clase, se declaran dos propiedades: `nombre` y `edad`. Las propiedades son variables que pertenecen a un objeto y pueden ser accedidas y modificadas a través de métodos especiales llamados getters y setters.

4. Métodos de la clase: A continuación, se define un método llamado `saludar` dentro de la clase. Los métodos son funciones que se pueden llamar para realizar una acción específica. El método `saludar` imprime un saludo personalizado utilizando el nombre y la edad del objeto.

5. Implementación del método `saludar`: La implementación del método `saludar` se proporciona después de la definición del método. La implementación utiliza la función `NSLog` para imprimir el saludo en la consola.

6. Función `main`: La función `main` es el punto de entrada del programa. Esta función crea una instancia de la clase `MyClass`, establece las propiedades del objeto y luego llama al método `saludar` del objeto.

7. Uso del autorelease pool: La función `@autoreleasepool` se utiliza para crear un autorelease pool, que es un ámbito en el que los objetos creados con `alloc` y `init` se liberan automáticamente al final del ámbito. Esto ayuda a evitar la acumulación de objetos en la memoria y las posibles fugas de memoria.