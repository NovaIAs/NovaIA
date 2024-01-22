```objective-c
// Importamos las librerías necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Definimos la clase principal de nuestra aplicación
@interface MiAplicacion : UIViewController
@end

// Implementamos la clase principal de nuestra aplicación
@implementation MiAplicacion

- (void)viewDidLoad {
    [super viewDidLoad];

    // Creamos una etiqueta para mostrar el texto "Hola, mundo!"
    UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(100, 100, 200, 200)];
    label.text = @"¡Hola, mundo!";
    label.textAlignment = NSTextAlignmentCenter;
    label.font = [UIFont systemFontOfSize:20];
    [self.view addSubview:label];

    // Creamos un botón para cerrar la aplicación
    UIButton *button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    button.frame = CGRectMake(100, 300, 200, 200);
    [button setTitle:@"Cerrar" forState:UIControlStateNormal];
    [button addTarget:self action:@selector(cerrarAplicacion:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:button];
}

- (void)cerrarAplicacion:(UIButton *)sender {
    exit(0);
}

@end

// Creamos una instancia de la clase principal de nuestra aplicación
MiAplicacion *miAplicacion = [[MiAplicacion alloc] init];

// Ejecutamos la aplicación
UIApplicationMain(argc, argv, nil, NSStringFromClass([MiAplicacion class]));
```

Este código crea una aplicación sencilla con una etiqueta que muestra el texto "Hola, mundo!" y un botón para cerrar la aplicación. La aplicación está escrita en Objective-C, que es un lenguaje de programación orientado a objetos utilizado para desarrollar aplicaciones para iOS y macOS.

El código está dividido en varias partes:

* La primera parte importa las librerías necesarias para crear la aplicación.
* La segunda parte define la clase principal de la aplicación, que es una subclase de `UIViewController`.
* La tercera parte implementa la clase principal de la aplicación. En este método, creamos una etiqueta para mostrar el texto "Hola, mundo!" y un botón para cerrar la aplicación.
* La cuarta parte crea una instancia de la clase principal de la aplicación.
* La quinta parte ejecuta la aplicación.

Este código es sólo un ejemplo sencillo de cómo crear una aplicación en Objective-C. Para desarrollar aplicaciones reales, necesitarás aprender más sobre el lenguaje y las herramientas de desarrollo de Apple.