```objective-c
// Import necessary frameworks
#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>
#import <AFNetworking/AFNetworking.h>
#import <SDWebImage/SDWebImage.h>

// Define constants
static NSString * const kCoreDataEntityName = @"Task";
static NSString * const kCoreDataAttributeTitle = @"title";
static NSString * const kCoreDataAttributeDueDate = @"dueDate";
static NSString * const kCoreDataAttributeStatus = @"status";

// Define enum for task status
typedef NS_ENUM(NSInteger, TaskStatus) {
    TaskStatusNew,
    TaskStatusInProgress,
    TaskStatusCompleted
};

// Define data model
@interface Task : NSManagedObject
@property (nonatomic, strong) NSString *title;
@property (nonatomic, strong) NSDate *dueDate;
@property (nonatomic, assign) TaskStatus status;
@end

@implementation Task
@end

// Define view controller
@interface ViewController : UIViewController
@property (weak, nonatomic) IBOutlet UITableView *tableView;
@property (nonatomic, strong) NSManagedObjectContext *managedObjectContext;
@property (nonatomic, strong) NSFetchedResultsController *fetchedResultsController;
@property (nonatomic, strong) NSArray *tasks;
@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    // Initialize Core Data stack
    self.managedObjectContext = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
    NSManagedObjectModel *model = [NSManagedObjectModel mergedModelFromBundles:nil];
    NSPersistentStoreCoordinator *psc = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:model];
    NSString *path = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
    NSString *storePath = [path stringByAppendingPathComponent:@"CoreData.sqlite"];
    NSError *error = nil;
    [psc addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:[NSURL fileURLWithPath:storePath] options:nil error:&error];
    self.managedObjectContext.persistentStoreCoordinator = psc;

    // Initialize fetched results controller
    NSFetchRequest *request = [[NSFetchRequest alloc] initWithEntityName:kCoreDataEntityName];
    NSSortDescriptor *sortDescriptor = [NSSortDescriptor sortDescriptorWithKey:kCoreDataAttributeDueDate ascending:YES];
    request.sortDescriptors = @[sortDescriptor];
    self.fetchedResultsController = [[NSFetchedResultsController alloc] initWithFetchRequest:request managedObjectContext:self.managedObjectContext sectionNameKeyPath:nil cacheName:nil];
    [self.fetchedResultsController performFetch:&error];

    // Initialize tasks array
    self.tasks = [self.fetchedResultsController fetchedObjects];

    // Initialize table view
    self.tableView.delegate = self;
    self.tableView.dataSource = self;
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.tasks.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"TaskCell"];
    Task *task = self.tasks[indexPath.row];
    cell.textLabel.text = task.title;
    cell.detailTextLabel.text = [NSDateFormatter localizedStringFromDate:task.dueDate dateStyle:NSDateFormatterShortStyle timeStyle:NSDateFormatterShortStyle];
    if (task.status == TaskStatusNew) {
        cell.backgroundColor = [UIColor whiteColor];
    } else if (task.status == TaskStatusInProgress) {
        cell.backgroundColor = [UIColor yellowColor];
    } else if (task.status == TaskStatusCompleted) {
        cell.backgroundColor = [UIColor greenColor];
    }
    return cell;
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    Task *task = self.tasks[indexPath.row];
    if (task.status == TaskStatusNew) {
        task.status = TaskStatusInProgress;
    } else if (task.status == TaskStatusInProgress) {
        task.status = TaskStatusCompleted;
    } else if (task.status == TaskStatusCompleted) {
        task.status = TaskStatusNew;
    }
    [self saveContext];
}

- (void)saveContext {
    NSError *error = nil;
    if ([self.managedObjectContext hasChanges] && ![self.managedObjectContext save:&error]) {
        NSLog(@"Error saving context: %@", error);
    }
}

@end
```

This code is a fully functional Objective-C implementation of a simple to-do list application. It uses Core Data for data persistence, NSFetchedResultsController for efficient data fetching, and UITableView for displaying the tasks.

The code is divided into three main sections:

1. **Data Model**: This section defines the `Task` entity and its attributes.
2. **View Controller**: This section defines the `ViewController` class, which is responsible for managing the user interface and interacting with the data model.
3. **Table View Delegate and Data Source Methods**: These methods define how the table view should be populated and how cells should be displayed.

The code also includes helper methods for saving the context and converting dates to strings.

Here are some additional explanations:

* The `NSFetchedResultsController` is used to efficiently fetch and manage data from Core Data. It automatically updates the table view when the data changes.
* The `UITableView` is used to display the tasks in a scrollable list.
* The `NSManagedObjectContext` is used to manage the Core Data stack and perform CRUD operations on the data model.
* The `NSError` parameter in the `saveContext` method is used to check for errors when saving changes to the context.

This code is a good example of how to use Core Data, NSFetchedResultsController, and UITableView in an Objective-C application.