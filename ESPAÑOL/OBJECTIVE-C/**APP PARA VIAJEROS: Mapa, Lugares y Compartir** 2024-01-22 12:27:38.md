```objective-c
#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>
#import <MapKit/MapKit.h>
#import <CoreLocation/CoreLocation.h>
#import <Social/Social.h>
#import <Accounts/Accounts.h>

@interface ViewController : UIViewController <MKMapViewDelegate, CLLocationManagerDelegate, SLComposeViewControllerDelegate> {
    MKMapView *mapView;
    CLLocationManager *locationManager;
    NSManagedObjectContext *managedObjectContext;
}

@property (nonatomic, strong) NSArray *locations;

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Crear el mapa
    mapView = [[MKMapView alloc] initWithFrame:self.view.bounds];
    mapView.delegate = self;
    [self.view addSubview:mapView];
    
    // Crear el administrador de localización
    locationManager = [[CLLocationManager alloc] init];
    locationManager.delegate = self;
    
    // Solicitar autorización de localización
    [locationManager requestWhenInUseAuthorization];
    
    // Crear el contexto de Core Data
    managedObjectContext = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
    NSManagedObjectModel *model = [NSManagedObjectModel mergedModelFromBundles:nil];
    managedObjectContext.persistentStoreCoordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:model];
    [managedObjectContext.persistentStoreCoordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:[NSURL fileURLWithPath:[[NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) lastObject] stringByAppendingPathComponent:@"DataModel.sqlite"]] options:nil error:nil];
    
    // Cargar las ubicaciones de Core Data
    NSFetchRequest *request = [[NSFetchRequest alloc] initWithEntityName:@"Location"];
    self.locations = [managedObjectContext executeFetchRequest:request error:nil];
    
    // Añadir anotaciones al mapa
    for (Location *location in self.locations) {
        MKPointAnnotation *annotation = [[MKPointAnnotation alloc] init];
        annotation.coordinate = CLLocationCoordinate2DMake([location.latitude floatValue], [location.longitude floatValue]);
        annotation.title = location.name;
        annotation.subtitle = location.address;
        [mapView addAnnotation:annotation];
    }
    
    // Añadir el botón de compartir
    UIBarButtonItem *shareButton = [[UIBarButtonItem alloc] initWithBarButtonSystemItem:UIBarButtonSystemItemAction target:self action:@selector(share)];
    self.navigationItem.rightBarButtonItem = shareButton;
}

- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
    
    // Iniciar la actualización de la localización
    [locationManager startUpdatingLocation];
}

- (void)viewDidDisappear:(BOOL)animated {
    [super viewDidDisappear:animated];
    
    // Detener la actualización de la localización
    [locationManager stopUpdatingLocation];
}

- (void)mapView:(MKMapView *)mapView didUpdateUserLocation:(MKUserLocation *)userLocation {
    // Centrar el mapa en la ubicación actual
    [mapView setCenterCoordinate:userLocation.coordinate animated:YES];
}

- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray *)locations {
    // Guardar la nueva ubicación en Core Data
    CLLocation *location = [locations lastObject];
    Location *newLocation = [NSEntityDescription insertNewObjectForEntityForName:@"Location" inManagedObjectContext:managedObjectContext];
    newLocation.latitude = @(location.coordinate.latitude);
    newLocation.longitude = @(location.coordinate.longitude);
    [managedObjectContext save:nil];
    
    // Añadir una anotación al mapa
    MKPointAnnotation *annotation = [[MKPointAnnotation alloc] init];
    annotation.coordinate = CLLocationCoordinate2DMake(location.coordinate.latitude, location.coordinate.longitude);
    annotation.title = @"Nueva ubicación";
    annotation.subtitle = @"Añadida automáticamente";
    [mapView addAnnotation:annotation];
}

- (void)share {
    // Crear el controlador de compartir
    SLComposeViewController *composeViewController = [SLComposeViewController composeViewControllerForServiceType:SLServiceTypeTwitter];
    composeViewController.delegate = self;
    
    // Añadir el texto y la imagen al compartir
    [composeViewController setInitialText:@"Estoy en un lugar fantástico!"];
    [composeViewController addImage:[UIImage imageNamed:@"photo.png"]];
    
    // Presentar el controlador de compartir
    [self presentViewController:composeViewController animated:YES completion:nil];
}

- (void)composeViewController:(SLComposeViewController *)composeViewController didFinishWithResult:(NSDictionary *)result {
    // Descartar el controlador de compartir
    [composeViewController dismissViewControllerAnimated:YES completion:nil];
}

@end
```

Este código crea una aplicación de iOS que muestra un mapa con la ubicación actual del usuario y las ubicaciones guardadas en Core Data. La aplicación también permite compartir la ubicación actual del usuario en Twitter.

El código está dividido en varios métodos:

* `viewDidLoad`: Este método se ejecuta cuando la vista se carga por primera vez. Crea el mapa, el administrador de localización, el contexto de Core Data y carga las ubicaciones de Core Data.
* `viewDidAppear`: Este método se ejecuta cuando la vista aparece. Inicia la actualización de la localización.
* `viewDidDisappear`: Este método se ejecuta cuando la vista desaparece. Detiene la actualización de la localización.
* `mapView:didUpdateUserLocation:`: Este método se ejecuta cuando la ubicación del usuario cambia. Centra el mapa en la nueva ubicación.
* `locationManager:didUpdateLocations:`: Este método se ejecuta cuando el administrador de localización actualiza la ubicación del usuario. Guarda la nueva ubicación en Core Data y añade una anotación al mapa.
* `share`: Este método se ejecuta cuando el usuario toca el botón de compartir. Crea el controlador de compartir y lo presenta.
* `composeViewController:didFinishWithResult:`: Este método se ejecuta cuando el usuario descarta el controlador de compartir. Descarta el controlador de compartir.