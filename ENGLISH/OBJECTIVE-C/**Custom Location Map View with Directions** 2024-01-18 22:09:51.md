# Import necessary Cocoa frameworks
```objective-c
#import <UIKit/UIKit.h>
#import <CoreLocation/CoreLocation.h>
#import <MapKit/MapKit.h>
```

## Define the interface for `CustomLocationViewController` class
```objective-c
@interface CustomLocationViewController : UIViewController <CLLocationManagerDelegate, MKMapViewDelegate>
```

## Implement the `CustomLocationViewController` class
```objective-c
@implementation CustomLocationViewController {
    CLLocationManager *locationManager;
    MKMapView *mapView;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Initialize the location manager
    locationManager = [[CLLocationManager alloc] init];
    locationManager.delegate = self;
    
    // Request permission to access the user's location
    [locationManager requestWhenInUseAuthorization];
    
    // Create the map view
    mapView = [[MKMapView alloc] initWithFrame:self.view.bounds];
    mapView.mapType = MKMapTypeStandard;
    mapView.delegate = self;
    
    // Add the map view to the view controller's view
    [self.view addSubview:mapView];
}

#pragma mark - CLLocationManagerDelegate methods
- (void)locationManager:(CLLocationManager *)manager didUpdateLocations:(NSArray<CLLocation *> *)locations {
    // Get the current location
    CLLocation *location = [locations lastObject];
    
    // Update the map view's camera to center on the current location
    MKCoordinateRegion region = MKCoordinateRegionMake(location.coordinate, MKCoordinateSpanMake(0.01, 0.01));
    [mapView setRegion:region animated:YES];
    
    // Add an annotation to the map view to indicate the current location
    MKPointAnnotation *annotation = [[MKPointAnnotation alloc] init];
    annotation.coordinate = location.coordinate;
    annotation.title = @"Current Location";
    [mapView addAnnotation:annotation];
}

#pragma mark - MKMapViewDelegate methods
- (void)mapView:(MKMapView *)mapView didSelectAnnotationView:(MKAnnotationView *)view {
    // When an annotation is selected, show a callout with more information
    MKAnnotationView *annotationView = mapView.selectedAnnotations[0];
    UIView *calloutView = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 200, 100)];
    calloutView.backgroundColor = [UIColor whiteColor];
    calloutView.layer.cornerRadius = 10;
    
    UILabel *titleLabel = [[UILabel alloc] initWithFrame:CGRectMake(10, 10, 180, 20)];
    titleLabel.text = annotationView.annotation.title;
    titleLabel.font = [UIFont boldSystemFontOfSize:16];
    [calloutView addSubview:titleLabel];
    
    UILabel *subtitleLabel = [[UILabel alloc] initWithFrame:CGRectMake(10, 30, 180, 20)];
    subtitleLabel.text = annotationView.annotation.subtitle;
    subtitleLabel.font = [UIFont systemFontOfSize:14];
    [calloutView addSubview:subtitleLabel];
    
    UIButton *directionsButton = [[UIButton alloc] initWithFrame:CGRectMake(10, 60, 180, 30)];
    directionsButton.backgroundColor = [UIColor blueColor];
    directionsButton.layer.cornerRadius = 5;
    [directionsButton setTitle:@"Get Directions" forState:UIControlStateNormal];
    [directionsButton addTarget:self action:@selector(getDirections:) forControlEvents:UIControlEventTouchUpInside];
    [calloutView addSubview:directionsButton];
    
    [annotationView addSubview:calloutView];
}

- (void)getDirections:(id)sender {
    // Get the current location
    CLLocation *currentLocation = locationManager.location;
    
    // Get the destination location from the selected annotation
    MKAnnotationView *annotationView = mapView.selectedAnnotations[0];
    CLLocationCoordinate2D destinationCoordinate = annotationView.annotation.coordinate;
    
    // Open the Maps app with directions from the current location to the destination location
    MKDirectionsRequest *request = [[MKDirectionsRequest alloc] init];
    request.source = [MKMapItem mapItemForCurrentLocation];
    request.destination = [MKMapItem mapItemForCoordinate:destinationCoordinate];
    MKDirections *directions = [[MKDirections alloc] initWithRequest:request];
    [directions calculateDirectionsWithCompletionHandler:^(MKDirectionsResponse *response, NSError *error) {
        if (error) {
            // Handle the error
        } else {
            MKRoute *route = response.routes[0];
            [mapView addOverlay:route.polyline];
            
            // Center the map view on the route
            MKCoordinateRegion region = MKCoordinateRegionMakeWithDistance(route.polyline.coordinate, 5000, 5000);
            [mapView setRegion:region animated:YES];
        }
    }];
}

@end
```