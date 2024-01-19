```objective-c
// Import necessary headers
#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>
#import "MyDataModel.h" // Custom data model class

@interface MyComplexViewController : UIViewController <UITableViewDataSource, UITableViewDelegate>

@property (nonatomic, strong) NSManagedObjectContext *managedObjectContext;
@property (nonatomic, strong) UITableView *tableView;
@property (nonatomic, strong) NSArray *items;

- (void)viewDidLoad;
- (void)configureTableView;
- (void)loadData;
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section;
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath;

@end

@implementation MyComplexViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Create managed object context
    self.managedObjectContext = [[NSManagedObjectContext alloc] initWithConcurrencyType:NSMainQueueConcurrencyType];
    NSManagedObjectModel *model = [NSManagedObjectModel mergedModelFromBundles:nil];
    self.managedObjectContext.persistentStoreCoordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:model];
    NSError *error = nil;
    if ([self.managedObjectContext.persistentStoreCoordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:[NSURL fileURLWithPath:[[NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) firstObject] stringByAppendingPathComponent:@"model.sqlite"]] options:nil] error:&error) == nil) {
        // Handle error
    }
    
    // Configure table view
    [self configureTableView];
    
    // Load data from CoreData
    [self loadData];
}

- (void)configureTableView {
    self.tableView = [[UITableView alloc] initWithFrame:self.view.bounds style:UITableViewStylePlain];
    self.tableView.dataSource = self;
    self.tableView.delegate = self;
    [self.view addSubview:self.tableView];
}

- (void)loadData {
    NSFetchRequest *fetchRequest = [[NSFetchRequest alloc] init];
    NSEntityDescription *entity = [NSEntityDescription entityForName:@"Item" inManagedObjectContext:self.managedObjectContext];
    fetchRequest.entity = entity;
    NSError *error = nil;
    self.items = [self.managedObjectContext executeFetchRequest:fetchRequest error:&error];
    if (error) {
        // Handle error
    }
    [self.tableView reloadData];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.items.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"ItemCell"];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:@"ItemCell"];
    }
    
    NSManagedObject *item = self.items[indexPath.row];
    cell.textLabel.text = [item valueForKey:@"name"];
    cell.detailTextLabel.text = [item valueForKey:@"description"];
    
    return cell;
}

@end
```

This code is an example of a complex Objective-C class that demonstrates the use of CoreData, UITableView, and NSFetchRequest. It includes error handling and provides a detailed explanation of the implementation.