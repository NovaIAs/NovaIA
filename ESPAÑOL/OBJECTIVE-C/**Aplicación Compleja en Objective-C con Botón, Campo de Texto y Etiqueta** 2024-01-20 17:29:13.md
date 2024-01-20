```objective-c
// Importa las bibliotecas necesarias
#import <UIKit/UIKit.h>

// Implementa una aplicación compleja en Objective-C

// Define una clase para la interfaz de usuario
@interface MiAplicacion : UIViewController {
    // Declarar variables y controles de la interfaz de usuario
    UIButton *boton;
    UITextField *campoTexto;
    UILabel *etiqueta;
}

// Implementa los métodos de la clase
@implementation MiAplicacion

// Define el método que se ejecuta cuando se carga la vista
- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Inicializa los controles de la interfaz de usuario
    boton = [[UIButton alloc] initWithFrame:CGRectMake(100, 100, 100, 50)];
    [boton setTitle:@"Presionar" forState:UIControlStateNormal];
    [boton addTarget:self action:@selector(botonPresionado:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:boton];
    
    campoTexto = [[UITextField alloc] initWithFrame:CGRectMake(100, 200, 100, 50)];
    campoTexto.placeholder = @"Ingresar texto";
    [self.view addSubview:campoTexto];
    
    etiqueta = [[UILabel alloc] initWithFrame:CGRectMake(100, 300, 100, 50)];
    [self.view addSubview:etiqueta];
}

// Define el método que se ejecuta cuando se presiona el botón
- (void)botonPresionado:(UIButton *)sender {
    // Obtener el texto ingresado en el campo de texto
    NSString *texto = campoTexto.text;
    
    // Actualizar la etiqueta con el texto ingresado
    etiqueta.text = texto;
}

// Fin de la implementación de la clase

@end

// Define el método principal de la aplicación
int main(int argc, char * argv[]) {
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([MiAplicacion class]));
    }
}
```

Explicación del código:

* La clase `MiAplicacion` es la clase principal para la interfaz de usuario de la aplicación. Hereda de la clase `UIViewController`, que es la clase base para los controladores de vista en UIKit.

* El método `viewDidLoad` se ejecuta cuando se carga la vista. Inicializa los controles de la interfaz de usuario, como el botón, el campo de texto y la etiqueta.

* El método `botonPresionado:` se ejecuta cuando se presiona el botón. Obtiene el texto ingresado en el campo de texto y lo actualiza en la etiqueta.

* El método `main` es el punto de entrada de la aplicación. Crea una instancia de la clase `UIApplication` y ejecuta el método `UIApplicationMain`, que inicializa la aplicación y carga la interfaz de usuario.