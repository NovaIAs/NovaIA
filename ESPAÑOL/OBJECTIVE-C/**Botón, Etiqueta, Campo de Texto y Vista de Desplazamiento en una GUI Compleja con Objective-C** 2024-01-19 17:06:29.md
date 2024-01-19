```objective-c
#import <UIKit/UIKit.h>

@interface MiClaseCompleja : UIViewController

@property(nonatomic, strong) UIButton *boton;
@property(nonatomic, strong) UILabel *etiqueta;
@property(nonatomic, strong) UITextView *vistaTexto;
@property(nonatomic, strong) UIScrollView *vistaDesplazamiento;

- (void)viewDidLoad;
- (void)botonPulsado:(UIButton *)boton;

@end

@implementation MiClaseCompleja

- (void)viewDidLoad {
  [super viewDidLoad];

  self.view.backgroundColor = [UIColor whiteColor];

  self.boton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
  self.boton.frame = CGRectMake(100, 100, 100, 50);
  [self.boton setTitle:@"Pulsar" forState:UIControlStateNormal];
  [self.boton addTarget:self action:@selector(botonPulsado:) forControlEvents:UIControlEventTouchUpInside];
  [self.view addSubview:self.boton];

  self.etiqueta = [[UILabel alloc] initWithFrame:CGRectMake(100, 200, 200, 50)];
  self.etiqueta.text = @"Etiqueta";
  [self.view addSubview:self.etiqueta];

  self.vistaTexto = [[UITextView alloc] initWithFrame:CGRectMake(100, 300, 300, 200)];
  self.vistaTexto.text = @"Vista de texto";
  [self.view addSubview:self.vistaTexto];

  self.vistaDesplazamiento = [[UIScrollView alloc] initWithFrame:CGRectMake(0, 0, self.view.frame.size.width, self.view.frame.size.height)];
  self.vistaDesplazamiento.contentSize = CGSizeMake(self.view.frame.size.width, self.view.frame.size.height * 2);
  [self.view addSubview:self.vistaDesplazamiento];

  [self.vistaDesplazamiento addSubview:self.boton];
  [self.vistaDesplazamiento addSubview:self.etiqueta];
  [self.vistaDesplazamiento addSubview:self.vistaTexto];
}

- (void)botonPulsado:(UIButton *)boton {
  self.etiqueta.text = @"¡Botón pulsado!";
}

@end
```

Este código crea una interfaz gráfica de usuario (GUI) compleja en Objective-C, utilizando UIKit. La GUI consiste en un botón, una etiqueta, un campo de texto y una vista de desplazamiento. Cuando se pulsa el botón, el texto de la etiqueta cambia a "¡Botón pulsado!".

El código está organizado en dos archivos:

* `MiClaseCompleja.h`: Este archivo contiene la definición de la clase `MiClaseCompleja`.
* `MiClaseCompleja.m`: Este archivo contiene la implementación de los métodos de la clase `MiClaseCompleja`.

El método `viewDidLoad` de la clase `MiClaseCompleja` se llama cuando se carga la vista. Este método crea la GUI y la añade a la vista. El método `botonPulsado:` de la clase `MiClaseCompleja` se llama cuando se pulsa el botón. Este método cambia el texto de la etiqueta a "¡Botón pulsado!".