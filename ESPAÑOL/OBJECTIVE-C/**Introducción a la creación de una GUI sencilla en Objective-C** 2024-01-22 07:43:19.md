**Introducción**

Este código crea una interfaz gráfica de usuario (GUI) sencilla en Objective-C, utilizando el framework Cocoa. La GUI consta de una ventana, un botón y una etiqueta. Cuando se hace clic en el botón, la etiqueta muestra el mensaje "Hola mundo!".

**Código**

```objective-c
#import <Cocoa/Cocoa.h>

@interface MiVentana : NSWindow
- (IBAction)clicBoton:(id)sender;
@end

@implementation MiVentana

- (id)initWithContentRect:(NSRect)contentRect styleMask:(NSWindowStyleMask)styleMask backing:(NSBackingStoreType)backing defer:(BOOL)deferCreation
{
    self = [super initWithContentRect:contentRect styleMask:styleMask backing:backing defer:deferCreation];
    if (self) {
        // Crear el botón
        NSButton *boton = [[NSButton alloc] initWithFrame:NSMakeRect(100, 100, 100, 30)];
        [boton setTitle:@"Clic aquí" alignment:NSCenterTextAlignment];
        [boton setTarget:self];
        [boton setAction:@selector(clicBoton:)];
        [self.contentView addSubview:boton];
        
        // Crear la etiqueta
        NSTextField *etiqueta = [[NSTextField alloc] initWithFrame:NSMakeRect(100, 150, 100, 30)];
        [etiqueta setEditable:NO];
        [etiqueta setStringValue:@"¡Hola mundo!"];
        [self.contentView addSubview:etiqueta];
    }
    return self;
}

- (void)clicBoton:(id)sender
{
    // Mostrar el mensaje "Hola mundo!" en la etiqueta
    NSTextField *etiqueta = (NSTextField *)[self.contentView viewWithTag:1];
    [etiqueta setStringValue:@"¡Hola mundo!"];
}

@end

int main(int argc, const char * argv[])
{
    NSApplication *app = [NSApplication sharedApplication];
    MiVentana *ventana = [[MiVentana alloc] initWithContentRect:NSMakeRect(200, 200, 400, 300) styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable defer:NO];
    [app run];
    return 0;
}
```

**Explicación**

* El código comienza importando el framework Cocoa, que es necesario para crear la GUI.

* La clase `MiVentana` es una subclase de `NSWindow`, que representa la ventana principal de la aplicación.

* El método `-initWithContentRect:styleMask:backing:defer:` es el método inicializador de la clase `MiVentana`. Este método crea la ventana con el tamaño y el estilo especificados.

* El método `-clicBoton:` es un método de acción que se llama cuando se hace clic en el botón. Este método muestra el mensaje "Hola mundo!" en la etiqueta.

* La función `main()` es el punto de entrada de la aplicación. Esta función crea la aplicación, la ventana principal y la ejecuta.

**Ejemplo de uso**

Para ejecutar este código, simplemente compílelo con el siguiente comando:

```
gcc -o mi_ventana mi_ventana.c -framework Cocoa
```

Luego, ejecute el archivo `mi_ventana`:

```
./mi_ventana
```

Esto abrirá una ventana con un botón y una etiqueta. Cuando haga clic en el botón, la etiqueta mostrará el mensaje "Hola mundo!".