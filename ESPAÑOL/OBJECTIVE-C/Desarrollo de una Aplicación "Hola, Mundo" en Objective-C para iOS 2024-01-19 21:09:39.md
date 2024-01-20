```objective-c
// importar las bibliotecas necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// definir la clase principal
@interface ViewController : UIViewController
@end

// implementar la clase principal
@implementation ViewController

// definir el método principal
- (void)viewDidLoad {
    [super viewDidLoad];
    
    // crear un botón
    UIButton *button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    button.frame = CGRectMake(100, 100, 100, 50);
    button.setTitle:@"Presiona aquí", forState:UIControlStateNormal);
    [button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:button];
    
    // crear una etiqueta
    UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(100, 200, 200, 50)];
    label.text = @"Hola, mundo!";
    label.textAlignment = NSTextAlignmentCenter;
    [self.view addSubview:label];
}

// definir el método que se ejecuta cuando se presiona el botón
- (void)buttonPressed:(id)sender {
    // cambiar el texto de la etiqueta
    UILabel *label = [self.view viewWithTag:100];
    label.text = @"¡Presionaste el botón!";
}

// definir el método que se ejecuta cuando se gira el dispositivo
- (void)willRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration {
    // cambiar el tamaño del botón y la etiqueta en función de la orientación del dispositivo
    if (toInterfaceOrientation == UIInterfaceOrientationLandscapeLeft || toInterfaceOrientation == UIInterfaceOrientationLandscapeRight) {
        button.frame = CGRectMake(200, 100, 100, 50);
        label.frame = CGRectMake(200, 200, 200, 50);
    } else {
        button.frame = CGRectMake(100, 100, 100, 50);
        label.frame = CGRectMake(100, 200, 200, 50);
    }
}

@end


// definir la aplicación principal
int main(int argc, char *argv[]) {
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}
```

Este código crea una sencilla aplicación para iOS que muestra un botón y una etiqueta. Cuando se presiona el botón, el texto de la etiqueta cambia a "¡Presionaste el botón!". La aplicación también cambia automáticamente el tamaño del botón y la etiqueta cuando se gira el dispositivo.

El código está dividido en tres partes:

* La definición de la clase principal, `ViewController`. Esta clase contiene el código que define la interfaz de usuario de la aplicación y el comportamiento de la misma.
* La implementación de la clase principal, `ViewController`. Esta implementación contiene el código que se ejecuta cuando se carga la interfaz de usuario, cuando se presiona el botón y cuando se gira el dispositivo.
* La definición de la aplicación principal, `main()`. Esta función es el punto de entrada de la aplicación. Crea un objeto `autoreleasepool` y llama a la función `UIApplicationMain()`, que se encarga de inicializar la aplicación y ejecutarla.

Aquí hay una explicación más detallada del código:

* La clase `ViewController` es una subclase de `UIViewController`. `UIViewController` es una clase base para todos los controladores de vista en iOS. Un controlador de vista es un objeto que gestiona una única vista en la interfaz de usuario.
* El método `viewDidLoad()` se llama cuando se carga la vista del controlador de vista. En este método, se crean el botón y la etiqueta y se añaden a la vista.
* El método `buttonPressed()` se llama cuando se presiona el botón. En este método, se cambia el texto de la etiqueta a "¡Presionaste el botón!".
* El método `willRotateToInterfaceOrientation()` se llama cuando el dispositivo se gira. En este método, se cambia el tamaño del botón y la etiqueta en función de la orientación del dispositivo.
* La función `main()` es el punto de entrada de la aplicación. En esta función, se crea un objeto `autoreleasepool` y se llama a la función `UIApplicationMain()`, que se encarga de inicializar la aplicación y ejecutarla.

Este código es sólo un ejemplo sencillo de cómo crear una aplicación para iOS. Hay muchos otros componentes y características que se pueden utilizar para crear aplicaciones más complejas.