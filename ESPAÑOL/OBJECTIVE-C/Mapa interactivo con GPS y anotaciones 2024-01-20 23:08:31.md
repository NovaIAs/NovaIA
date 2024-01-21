```objective-c
// Importamos las bibliotecas necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>
#import <AVFoundation/AVFoundation.h>
#import <CoreLocation/CoreLocation.h>
#import <MapKit/MapKit.h>

// Creamos una clase que se encargará de gestionar la interfaz de usuario
@interface MiControlador : UIViewController <MKMapViewDelegate>
{
    // Creamos un mapa
    MKMapView *mapa;

    // Creamos un localizador para obtener la ubicación actual
    CLLocationManager *localizador;
}

// Implementamos el método que se encarga de inicializar la interfaz de usuario
- (void)viewDidLoad
{
    // Llamamos al método [super viewDidLoad] para inicializar la interfaz de usuario
    [super viewDidLoad];

    // Creamos el mapa
    mapa = [[MKMapView alloc] initWithFrame:self.view.bounds];
    mapa.delegate = self;
    [self.view addSubview:mapa];

    // Creamos el localizador
    localizador = [[CLLocationManager alloc] init];
    localizador.delegate = self;
    [localizador requestWhenInUseAuthorization];
}

// Implementamos el método que se encarga de actualizar la ubicación actual
- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray<CLLocation *> *)locations
{
    // Obtenemos la última ubicación
    CLLocation *ubicacion = [locations lastObject];

    // Creamos una región centrada en la última ubicación
    MKCoordinateRegion region = MKCoordinateRegionMakeWithDistance(ubicacion.coordinate, 1000, 1000);

    // Mostramos la región en el mapa
    [mapa setRegion:region animated:YES];
}

// Implementamos el método que se encarga de gestionar los errores de localización
- (void)locationManager:(CLLocationManager *)manager didFailWithError:(NSError *)error
{
    // Mostramos un mensaje de error
    UIAlertController *alerta = [UIAlertController alertControllerWithTitle:@"Error" message:@"No se pudo obtener la ubicación actual" preferredStyle:UIAlertControllerStyleAlert];
    UIAlertAction *accion = [UIAlertAction actionWithTitle:@"Aceptar" style:UIAlertActionStyleDefault handler:nil];
    [alerta addAction:accion];
    [self presentViewController:alerta animated:YES completion:nil];
}

// Implementamos el método que se encarga de gestionar los cambios en la región del mapa
- (void)mapView:(MKMapView *)mapView regionDidChangeAnimated:(BOOL)animated
{
    // Obtenemos la región actual del mapa
    MKCoordinateRegion region = mapa.region;

    // Creamos una nueva región con un radio menor
    MKCoordinateRegion nuevaRegion = MKCoordinateRegionMakeWithDistance(region.center, region.span.latitudeDelta / 2, region.span.longitudeDelta / 2);

    // Mostramos la nueva región en el mapa
    [mapa setRegion:nuevaRegion animated:YES];
}

// Implementamos el método que se encarga de gestionar las pulsaciones en el mapa
- (void)mapView:(MKMapView *)mapView didSelectAnnotationView:(MKAnnotationView *)view
{
    // Obtenemos la anotación seleccionada
    MKAnnotation *anotacion = view.annotation;

    // Creamos una alerta con el título y la descripción de la anotación
    UIAlertController *alerta = [UIAlertController alertControllerWithTitle:anotacion.title message:anotacion.subtitle preferredStyle:UIAlertControllerStyleAlert];

    // Creamos una acción para cerrar la alerta
    UIAlertAction *accion = [UIAlertAction actionWithTitle:@"Aceptar" style:UIAlertActionStyleDefault handler:nil];

    // Añadimos la acción a la alerta
    [alerta addAction:accion];

    // Mostramos la alerta
    [self presentViewController:alerta animated:YES completion:nil];
}

@end

// Creamos una clase que se encargará de gestionar las anotaciones
@interface MiAnotacion : NSObject <MKAnnotation>
{
    // Creamos una propiedad para el título de la anotación
    NSString *titulo;

    // Creamos una propiedad para la descripción de la anotación
    NSString *descripcion;

    // Creamos una propiedad para la coordenada de la anotación
    CLLocationCoordinate2D coordenada;
}

// Implementamos el método que devuelve el título de la anotación
- (NSString *)title
{
    return titulo;
}

// Implementamos el método que devuelve la descripción de la anotación
- (NSString *)subtitle
{
    return descripcion;
}

// Implementamos el método que devuelve la coordenada de la anotación
- (CLLocationCoordinate2D)coordinate
{
    return coordenada;
}

// Creamos un inicializador para la anotación
- (instancetype)initWithTitulo:(NSString *)titulo descripcion:(NSString *)descripcion coordenada:(CLLocationCoordinate2D)coordenada
{