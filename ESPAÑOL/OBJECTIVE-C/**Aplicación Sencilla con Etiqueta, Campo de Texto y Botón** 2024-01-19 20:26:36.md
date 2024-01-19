```objective-c
// Importación de bibliotecas estándar
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Definición de la interfaz de la clase principal
@interface Principal : UIViewController
{
    // Declaración de variables privadas
    UILabel *etiqueta; // Etiqueta para mostrar texto
    UITextField *campoDeTexto; // Campo de texto para entrada de usuario
    UIButton *boton; // Botón para realizar una acción
}

@end

// Implementación de la clase principal
@implementation Principal

// Método que se ejecuta cuando se carga la vista
- (void)viewDidLoad
{
    [super viewDidLoad];
    
    // Creación y configuración de la etiqueta
    etiqueta = [[UILabel alloc] initWithFrame:CGRectMake(20, 20, 200, 20)];
    etiqueta.text = @"Hola mundo!";
    etiqueta.textColor = [UIColor blackColor];
    
    // Añadir la etiqueta a la vista
    [self.view addSubview:etiqueta];
    
    // Creación y configuración del campo de texto
    campoDeTexto = [[UITextField alloc] initWithFrame:CGRectMake(20, 60, 200, 20)];
    campoDeTexto.placeholder = @"Escribe algo aquí";
    campoDeTexto.borderStyle = UITextBorderStyleRoundedRect;
    
    // Añadir el campo de texto a la vista
    [self.view addSubview:campoDeTexto];
    
    // Creación y configuración del botón
    boton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    boton.frame = CGRectMake(20, 100, 200, 20);
    boton.setTitle:@"Presionar", forState:UIControlStateNormal);
    
    // Añadir el botón a la vista
    [self.view addSubview:boton];
    
    // Asignación de la acción a realizar cuando se presiona el botón
    [boton addTarget:self action:@selector(botonPresionado:) forControlEvents:UIControlEventTouchUpInside];
}

// Método que se ejecuta cuando se presiona el botón
- (void)botonPresionado:(id)sender
{
    // Obtener el texto ingresado en el campo de texto
    NSString *texto = campoDeTexto.text;
    
    // Actualizar el texto de la etiqueta con el texto ingresado
    etiqueta.text = texto;
}

// Método que se ejecuta cuando la vista está a punto de aparecer en la pantalla
- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
    
    // Ocultar la barra de navegación
    [self.navigationController setNavigationBarHidden:YES animated:YES];
}

// Método que se ejecuta cuando la vista está a punto de desaparecer de la pantalla
- (void)viewWillDisappear:(BOOL)animated
{
    [super viewWillDisappear:animated];
    
    // Mostrar la barra de navegación
    [self.navigationController setNavigationBarHidden:NO animated:YES];
}

@end
```

Explicación del código:

1. **Importación de bibliotecas estándar:** Se importan las bibliotecas estándar de la plataforma iOS, incluyendo la biblioteca `Foundation` para funciones básicas y la biblioteca `UIKit` para la interfaz de usuario.

2. **Definición de la interfaz de la clase principal:** Se define la interfaz de la clase principal `Principal`, que hereda de `UIViewController`. Esta clase representa la vista principal de la aplicación.

3. **Implementación de la clase principal:** Se implementa la clase principal `Principal`, que contiene los métodos que se ejecutan cuando se carga la vista, cuando se presiona el botón, cuando la vista está a punto de aparecer en la pantalla y cuando está a punto de desaparecer de la pantalla.

4. **Creación y configuración de la etiqueta, el campo de texto y el botón:** Se crean y configuran los controles de la interfaz de usuario, incluyendo una etiqueta para mostrar texto, un campo de texto para entrada de usuario y un botón para realizar una acción.

5. **Asignación de la acción a realizar cuando se presiona el botón:** Se asigna la acción `botonPresionado:` al botón, que se ejecuta cuando el usuario presiona el botón.

6. **Métodos que se ejecutan en diferentes etapas del ciclo de vida de la vista:** Se definen los métodos `viewDidLoad`, `viewWillAppear:` y `viewWillDisappear:` para realizar acciones específicas en diferentes etapas del ciclo de vida de la vista.

Este código crea una aplicación simple que muestra una etiqueta, un campo de texto y un botón. Cuando el usuario escribe algo en el campo de texto y presiona el botón, el texto ingresado se muestra en la etiqueta.