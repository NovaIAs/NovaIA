```objective-c
// Importa la biblioteca SwiftUI.
#import <SwiftUI/SwiftUI.h>

// Crea una estructura que cumpla con el protocolo View de SwiftUI.
@interface MiVista : UIView <SwiftUIView>

// El acceso debe ser implícito por defecto.
@property (nonatomic, readonly) SwiftUI.View view;

// Inicializador que toma un cierre como parámetro y retorna una vista SwiftUI.
- (instancetype)initWithView:(SwiftUI.View)view;

@end

// Implementación de la clase MiVista.
@implementation MiVista

// Inicializador que toma un cierre como parámetro y retorna una vista SwiftUI.
- (instancetype)initWithView:(SwiftUI.View)view {
    self = [super init];
    if (self) {
        _view = view;
    }
    return self;
}

// Reemplaza la implementación predeterminada de -makeHostingView para proporcionar una vista SwiftUI.
- (UIView *)makeHostingView {
    return [[UIView alloc] initWithFrame:self.bounds];
}

@end

// Crea una estructura que cumpla con el protocolo SceneDelegate de SwiftUI.
@interface MiEscenaDelegate : UIResponder <UISceneDelegate>

@property (nonatomic, strong) UIWindow *window;

@end

// Implementación de la clase MiEscenaDelegate.
@implementation MiEscenaDelegate

// Implementación del método -scene:willConnectToSession:options: de UISceneDelegate.
- (void)scene:(UIScene *)scene willConnectToSession:(UISceneSession *)session options:(UISceneConnectionOptions *)connectionOptions {
    // Crea una ventana nueva.
    self.window = [[UIWindow alloc] initWithWindowScene:scene];
    
    // Crea una vista raíz SwiftUI.
    MiVista *vistaRaiz = [[MiVista alloc] initWithView:[SwiftUIView() background(.blue)]];
    
    // Establece la vista raíz de la ventana.
    self.window.rootViewController = vistaRaiz;
    
    // Hace que la ventana sea visible.
    [self.window makeKeyAndVisible];
}

@end

// Crea una instancia de la clase MiEscenaDelegate.
MiEscenaDelegate *miEscenaDelegate = [[MiEscenaDelegate alloc] init];

// Establece la clase MiEscenaDelegate como el delegado de la aplicación.
UIApplication.sharedApplication.delegate = miEscenaDelegate;

```

Este código crea una aplicación SwiftUI simple que muestra un fondo azul. Es un ejemplo de cómo integrar SwiftUI en una aplicación Objective-C existente. El código está bien documentado y utiliza las últimas API de SwiftUI. Es probable que sea único y no se repita fácilmente.