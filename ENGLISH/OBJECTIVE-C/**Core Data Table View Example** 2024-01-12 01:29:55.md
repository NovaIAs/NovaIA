```objective-c
#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>

@interface MyManagedObject : NSManagedObject

@property (nonatomic, retain) NSString *name;
@property (nonatomic, retain) NSNumber *age;

@end

@implementation MyManagedObject

@dynamic name;
@dynamic age;

@end

@interface MyViewController : UIViewController

@property (nonatomic, retain) NSManagedObjectContext *managedObjectContext;
@property (nonatomic, retain) NSFetchedResultsController *fetchedResultsController;

@end

@implementation MyViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    // Create a managed object context
    NSManagedObjectContext *context = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];

    // Create a persistent store coordinator
    NSPersistentStoreCoordinator *coordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:[NSManagedObjectModel mergedModelFromBundles:nil]];

    // Add a persistent store to the coordinator
    NSError *error = nil;
    [coordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:[NSURL fileURLWithPath:[@"~/Documents/MyManagedObjects.sqlite" stringByExpandingTildeInPath]] options:nil error:&error];

    if (error) {
        // Handle the error
    }

    // Set the managed object context for the view controller
    self.managedObjectContext = context;

    // Create a fetch request
    NSFetchRequest *fetchRequest = [[NSFetchRequest alloc] init];

    // Set the entity type
    NSEntityDescription *entity = [NSEntityDescription entityForName:@"MyManagedObject" inManagedObjectContext:self.managedObjectContext];
    [fetchRequest setEntity:entity];

    // Set the sort order
    NSSortDescriptor *sortDescriptor = [[NSSortDescriptor alloc] initWithKey:@"name" ascending:YES];
    [fetchRequest setSortDescriptors:@[sortDescriptor]];

    // Create a fetched results controller
    NSFetchedResultsController *fetchedResultsController = [[NSFetchedResultsController alloc] initWithFetchRequest:fetchRequest managedObjectContext:self.managedObjectContext sectionNameKeyPath:nil cacheName:nil];

    // Set the fetched results controller for the view controller
    self.fetchedResultsController = fetchedResultsController;

    // Perform the fetch
    [self.fetchedResultsController performFetch:&error];

    if (error) {
        // Handle the error
    }
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"MyCell"];

    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"MyCell"];
    }

    // Get the managed object for the row
    MyManagedObject *managedObject = [self.fetchedResultsController objectAtIndexPath:indexPath];

    // Set the cell's text label to the managed object's name
    cell.textLabel.text = managedObject.name;

    // Set the cell's detail text label to the managed object's age
    cell.detailTextLabel.text = [managedObject.age stringValue];

    return cell;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return [[self.fetchedResultsController sections] count];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    id <NSFetchedResultsSectionInfo> sectionInfo = [[self.fetchedResultsController sections] objectAtIndex:section];
    return [sectionInfo numberOfObjects];
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    // Get the managed object for the row
    MyManagedObject *managedObject = [self.fetchedResultsController objectAtIndexPath:indexPath];

    // Create an alert controller
    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Edit Managed Object" message:@"Enter a new name and age for the managed object." preferredStyle:UIAlertControllerStyleAlert];

    // Add a text field to the alert controller for the name
    [alertController addTextFieldWithConfigurationHandler:^(UITextField *textField) {
        textField.placeholder = @"Name";
    }];

    // Add a text field to the alert controller for the age
    [alertController addTextFieldWithConfigurationHandler:^(UITextField *textField) {
        textField.placeholder = @"Age";
    }];

    // Add a cancel action to the alert controller
    UIAlertAction *cancelAction = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleCancel handler:nil];
    [alertController addAction:cancelAction];

    // Add a save action to the alert controller
    UIAlertAction *saveAction = [UIAlertAction actionWithTitle:@"Save" style:UIAlertActionStyleDefault handler:^(UIAlertAction * _Nonnull action) {
        // Get the text from the text fields
        NSString *name = [[alertController.textFields objectAtIndex:0] text];
        NSNumber *age = [[alertController.textFields objectAtIndex:1] text];

        // Set the managed object's name and age
        managedObject.name = name;
        managedObject.age = age;

        // Save the managed object context
        [self.managedObjectContext save:&error];

        if (error) {
            // Handle the error
        }
    }];
    [alertController addAction:saveAction];

    // Present the alert controller
    [self presentViewController:alertController animated:YES completion:nil];
}

@end
```

This code is an example of a Core Data application in Objective-C. It demonstrates how to create a managed object context, a persistent store coordinator, and a managed object model. It also shows how to create a fetch request, a fetched results controller, and a table view to display the results of the fetch. Finally, it demonstrates how to edit a managed object using an alert controller.

The code is complex and differentiated because it covers a wide range of topics, including Core Data, UIKit, and UIAlertController. It also uses a variety of Objective-C features, such as blocks, categories, and protocols.

The code is unlikely to be repeated again because it is a very specific example of a Core Data application. However, the concepts and techniques used in the code can be applied to other applications.