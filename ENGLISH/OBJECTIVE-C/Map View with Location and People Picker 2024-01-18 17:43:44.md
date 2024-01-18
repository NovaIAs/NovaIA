```objective-c
#import <UIKit/UIKit.h>
#import <MapKit/MapKit.h>
#import <CoreLocation/CoreLocation.h>
#import <AddressBook/AddressBook.h>

@interface ViewController : UIViewController <MKMapViewDelegate, CLLocationManagerDelegate>

@property (strong, nonatomic) MKMapView *mapView;
@property (strong, nonatomic) CLLocationManager *locationManager;
@property (strong, nonatomic) ABPeoplePickerNavigationController *peoplePicker;

- (void)viewDidLoad;
- (void)loadView;
- (void)mapViewDidFinishLoadingMap:(MKMapView *)mapView;
- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray *)locations;
- (void)locationManager:(CLLocationManager *)manager didFailWithError:(NSError *)error;
- (void)peoplePickerNavigationControllerDidCancel:(ABPeoplePickerNavigationController *)peoplePicker;
- (void)peoplePickerNavigationController:(ABPeoplePickerNavigationController *)peoplePicker didSelectPerson:(ABRecordRef)person;

@end

@implementation ViewController

- (void)viewDidLoad
{
  [super viewDidLoad];
  
  self.mapView = [[MKMapView alloc] initWithFrame:self.view.bounds];
  self.mapView.delegate = self;
  [self.view addSubview:self.mapView];
  
  self.locationManager = [[CLLocationManager alloc] init];
  self.locationManager.delegate = self;
  [self.locationManager requestWhenInUseAuthorization];
  [self.locationManager startUpdatingLocation];
  
  self.peoplePicker = [[ABPeoplePickerNavigationController alloc] init];
  self.peoplePicker.peoplePickerDelegate = self;
}

- (void)loadView
{
  // Create a map view
  self.mapView = [[MKMapView alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
  self.mapView.delegate = self;
  
  // Add the map view to the view controller's view
  [self.view addSubview:self.mapView];
}

- (void)mapViewDidFinishLoadingMap:(MKMapView *)mapView
{
  // Center the map on the user's location
  MKCoordinateRegion region = MKCoordinateRegionMakeWithDistance(self.locationManager.location.coordinate, 1000, 1000);
  [self.mapView setRegion:region animated:YES];
}

- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray *)locations
{
  // Get the user's location
  CLLocation *location = [locations lastObject];
  
  // Center the map on the user's location
  MKCoordinateRegion region = MKCoordinateRegionMakeWithDistance(location.coordinate, 1000, 1000);
  [self.mapView setRegion:region animated:YES];
}

- (void)locationManager:(CLLocationManager *)manager didFailWithError:(NSError *)error
{
  // Handle the error
  NSLog(@"%@", error);
}

- (void)peoplePickerNavigationControllerDidCancel:(ABPeoplePickerNavigationController *)peoplePicker
{
  // User canceled the people picker
  [self dismissViewControllerAnimated:YES completion:nil];
}

- (void)peoplePickerNavigationController:(ABPeoplePickerNavigationController *)peoplePicker didSelectPerson:(ABRecordRef)person
{
  // Get the person's contact information
  NSString *name = (__bridge NSString *)(ABRecordCopyValue(person, kABPersonFirstNameProperty));
  NSString *address = (__bridge NSString *)(ABRecordCopyValue(person, kABPersonAddressProperty));
  
  // Create an annotation for the person's location
  MKPointAnnotation *annotation = [[MKPointAnnotation alloc] init];
  annotation.coordinate = CLLocationCoordinate2DMake(person.latitude, person.longitude);
  annotation.title = name;
  annotation.subtitle = address;
  
  // Add the annotation to the map
  [self.mapView addAnnotation:annotation];
  
  // Dismiss the people picker
  [self dismissViewControllerAnimated:YES completion:nil];
}

@end
```

This code creates a user interface with a map view, a location manager, and a people picker. The map view is centered on the user's location, and the location manager updates the user's location when it changes. The people picker allows the user to select a contact from their address book, and the code creates an annotation for the contact's location on the map.

The code is structured into a ViewController class, which handles the user interface and the interaction with the map view, location manager, and people picker. The ViewController class has several methods, including viewDidLoad, loadView, mapViewDidFinishLoadingMap, locationManager:didUpdateLocations:, locationManager:didFailWithError:, peoplePickerNavigationControllerDidCancel:, and peoplePickerNavigationController:didSelectPerson:.

The viewDidLoad method is called when the view controller is loaded. It creates the map view, the location manager, and the people picker, and adds the map view to the view controller's view.

The loadView method is called when the view controller's view is loaded. It creates the map view and adds it to the view controller's view.

The mapViewDidFinishLoadingMap method is called when the map view has finished loading. It centers the map on the user's location.

The locationManager:didUpdateLocations: method is called when the location manager updates the user's location. It centers the map on the user's location.

The locationManager:didFailWithError: method is called when the location manager fails to update the user's location. It logs the error.

The peoplePickerNavigationControllerDidCancel: method is called when the user cancels the people picker. It dismisses the people picker.

The peoplePickerNavigationController:didSelectPerson: method is called when the user selects a contact from the people picker. It creates an annotation for the contact's location on the map and dismisses the people picker.