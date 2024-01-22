```objective-c
// Importamos las clases necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>
#import <MapKit/MapKit.h>

// Definimos la clase principal de nuestra aplicación
@interface MiAplicacion : NSObject <MKMapViewDelegate>

// Creamos una propiedad para el mapa
@property (nonatomic, strong) MKMapView *mapa;

// Creamos un método para crear la interfaz de usuario
- (void)crearInterfazUsuario;

// Creamos un método para manejar los eventos del mapa
- (void)mapaView:(MKMapView *)mapaView didSelectAnnotationView:(MKAnnotationView *)vistaDeAnotación;

@end

// Implementamos la clase principal
@implementation MiAplicacion

// Este método se llama cuando se inicia la aplicación
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // Creamos la ventana de la aplicación
    self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    
    // Creamos el controlador de la vista raíz
    UIViewController *controladorDeVistaRaiz = [[UIViewController alloc] init];
    
    // Creamos el mapa
    self.mapa = [[MKMapView alloc] initWithFrame:self.window.bounds];
    self.mapa.delegate = self;
    [controladorDeVistaRaiz.view addSubview:self.mapa];
    
    // Creamos la anotación
    MKPointAnnotation *anotación = [[MKPointAnnotation alloc] init];
    anotación.coordinate = CLLocationCoordinate2DMake(40.4166, -3.7033);
    anotación.title = @"Madrid";
    anotación.subtitle = @"Capital de España";
    [self.mapa addAnnotation:anotación];
    
    // Mostramos el controlador de la vista raíz
    self.window.rootViewController = controladorDeVistaRaíz;
    
    // Hacemos visible la ventana
    [self.window makeKeyAndVisible];
    
    return YES;
}

// Este método se llama cuando el usuario selecciona una anotación en el mapa
- (void)mapaView:(MKMapView *)mapaView didSelectAnnotationView:(MKAnnotationView *)vistaDeAnotación {
    // Recuperamos la anotación seleccionada
    MKAnnotation *anotación = vistaDeAnotación.annotation;
    
    // Mostramos una alerta con el título y el subtítulo de la anotación
    UIAlertController *alerta = [UIAlertController alertControllerWithTitle:anotación.title message:anotación.subtitle preferredStyle:UIAlertControllerStyleAlert];
    [alerta addAction:[UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:nil]];
    [self.window.rootViewController presentViewController:alerta animated:YES completion:nil];
}

@end

// Definimos la clase principal de la aplicación
int main(int argc, char * argv[]) {
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([MiAplicacion class]));
    }
}
```

**Explicación del código:**

1. Importamos las clases necesarias para crear la aplicación.
2. Creamos la clase principal de nuestra aplicación, `MiAplicacion`, que es un objeto que implementa el protocolo `MKMapViewDelegate`.
3. Creamos una propiedad para el mapa, `mapa`.
4. Creamos un método para crear la interfaz de usuario, `crearInterfazUsuario`.
5. Creamos un método para manejar los eventos del mapa, `mapaView:didSelectAnnotationView:`.
6. Implementamos la clase principal, `MiAplicacion`.
7. En el método `application:didFinishLaunchingWithOptions:`, creamos la ventana de la aplicación, el controlador de la vista raíz, el mapa y la anotación.
8. Mostramos el controlador de la vista raíz y hacemos visible la ventana.
9. En el método `mapaView:didSelectAnnotationView:`, recuperamos la anotación seleccionada y mostramos una alerta con el título y el subtítulo de la anotación.
10. Definimos la clase principal de la aplicación, `main`, que es el punto de entrada de la aplicación.