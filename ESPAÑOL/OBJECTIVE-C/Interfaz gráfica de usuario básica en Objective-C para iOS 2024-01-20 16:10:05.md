```objective-c
// Importamos las bibliotecas necesarias para el código
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Creamos las clases principales del código
@interface ViewController : UIViewController
{
    IBOutlet UILabel *etiquetaTexto;
}
@end

@implementation ViewController

// Método que se llama cuando se carga la vista
- (void)viewDidLoad {
    [super viewDidLoad];

    // Creamos una etiqueta y la añadimos a la vista
    etiquetaTexto = [[UILabel alloc] init];
    etiquetaTexto.frame = CGRectMake(100.0, 100.0, 200.0, 30.0);
    etiquetaTexto.text = @"Hola, mundo!";
    [self.view addSubview:etiquetaTexto];

    // Creamos un botón y lo añadimos a la vista
    UIButton *boton = [UIButton buttonWithType:UIButtonTypeSystem];
    boton.frame = CGRectMake(100.0, 150.0, 200.0, 30.0);
    [boton setTitle:@"Púlsame" forState:UIControlStateNormal];
    [boton addTarget:self action:@selector(botonPulsado:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:boton];
}

// Método que se llama cuando se pulsa el botón
- (void)botonPulsado:(id)sender {
    // Cambiamos el texto de la etiqueta
    etiquetaTexto.text = @"¡Has pulsado el botón!";
}

@end

// Creamos la aplicación principal
int main(int argc, char *argv[]) {
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}
```

Este código crea una sencilla aplicación iOS con una etiqueta y un botón. Cuando se pulsa el botón, el texto de la etiqueta cambia.

El código está dividido en tres partes principales:

1. La clase `ViewController` que es la clase principal de la aplicación.
2. El método `viewDidLoad` que se llama cuando se carga la vista.
3. El método `botonPulsado` que se llama cuando se pulsa el botón.

El código está bien estructurado y es fácil de leer y comprender. También es bastante eficiente y no debería haber ningún problema de rendimiento.

El código está en español para que sea más fácil de entender para los hablantes de español.