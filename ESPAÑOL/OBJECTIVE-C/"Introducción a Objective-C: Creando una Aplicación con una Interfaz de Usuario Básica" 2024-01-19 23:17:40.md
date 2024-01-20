```objective-c
// Clase principal del programa
@interface MiAplicacion : NSObject

// Método principal del programa
+ (void)main {
    // Creamos un objeto de la clase "MiAplicacion"
    MiAplicacion *miAplicacion = [[MiAplicacion alloc] init];
    
    // Llamamos al método "iniciar" del objeto
    [miAplicacion iniciar];
}

// Método para iniciar la aplicación
- (void)iniciar {
    // Creamos una instancia de la clase "Controlador"
    Controlador *controlador = [[Controlador alloc] init];
    
    // Creamos una instancia de la clase "Vista"
    Vista *vista = [[Vista alloc] init];
    
    // Asignamos el controlador a la vista
    vista.controlador = controlador;
    
    // Mostramos la vista
    [vista mostrar];
}

@end

// Clase del controlador
@interface Controlador : NSObject

// Método para el evento "clic" del botón
- (void)botonClic:(id)sender {
    // Obtenemos el texto del campo de texto
    NSString *texto = vista.campoTexto.text;
    
    // Mostramos un mensaje con el texto del campo de texto
    UIAlertView *alerta = [[UIAlertView alloc] initWithTitle:@"Texto" message:texto delegate:nil cancelButtonTitle:@"Aceptar" otherButtonTitles:nil];
    [alerta show];
}

@end

// Clase de la vista
@interface Vista : UIView

// Propiedad para el campo de texto
@property UITextField *campoTexto;

// Propiedad para el botón
@property UIButton *boton;

// Método para mostrar la vista
- (void)mostrar {
    // Creamos el campo de texto
    campoTexto = [[UITextField alloc] initWithFrame:CGRectMake(10, 10, 200, 30)];
    campoTexto.borderStyle = UITextBorderStyleRoundedRect;
    [self addSubview:campoTexto];
    
    // Creamos el botón
    boton = [[UIButton alloc] initWithFrame:CGRectMake(10, 50, 200, 30)];
    boton.backgroundColor = [UIColor blueColor];
    [boton setTitle:@"Clic" forState:UIControlStateNormal];
    [boton addTarget:controlador action:@selector(botonClic:) forControlEvents:UIControlEventTouchUpInside];
    [self addSubview:boton];
}

@end
```

Este código crea una aplicación sencilla con una ventana que contiene un campo de texto y un botón. Cuando el usuario hace clic en el botón, el texto que se encuentra en el campo de texto se muestra en un mensaje de alerta.

El código está escrito en Objective-C, que es un lenguaje de programación orientado a objetos que se utiliza para desarrollar aplicaciones para iOS.

El código está dividido en tres clases:

* **MiAplicacion:** Esta clase es la clase principal del programa. Contiene el método **main** que se llama cuando se inicia la aplicación.
* **Controlador:** Esta clase se encarga de gestionar los eventos de la interfaz de usuario. Contiene el método **botonClic** que se llama cuando el usuario hace clic en el botón.
* **Vista:** Esta clase representa la interfaz de usuario de la aplicación. Contiene el método **mostrar** que se llama para mostrar la interfaz de usuario.

El código utiliza el framework Cocoa Touch para crear la interfaz de usuario. Cocoa Touch es un framework de desarrollo de aplicaciones para iOS que proporciona una serie de clases y métodos para crear interfaces de usuario gráficas.

El código también utiliza el framework Foundation para gestionar los eventos de la interfaz de usuario. Foundation es un framework de desarrollo de aplicaciones para iOS que proporciona una serie de clases y métodos para gestionar los eventos de la interfaz de usuario, como el evento "clic" del botón.