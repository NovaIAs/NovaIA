```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a data source.
@protocol MyDataSource <NSObject>

- (NSInteger)numberOfItems;
- (id)itemAtIndex:(NSInteger)index;

@end

// Implement a data source that provides an array of strings.
@interface MyArrayDataSource : NSObject <MyDataSource>

@property (nonatomic, strong) NSArray *items;

- (instancetype)initWithItems:(NSArray *)items;

@end

@implementation MyArrayDataSource

- (instancetype)initWithItems:(NSArray *)items {
    self = [super init];
    if (self) {
        _items = items;
    }
    return self;
}

- (NSInteger)numberOfItems {
    return self.items.count;
}

- (id)itemAtIndex:(NSInteger)index {
    return self.items[index];
}

@end

// Implement a view controller that displays a table view.
@interface MyViewController : UIViewController <UITableViewDataSource, UITableViewDelegate>

@property (nonatomic, strong) UITableView *tableView;
@property (nonatomic, strong) MyDataSource *dataSource;

@end

@implementation MyViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    // Initialize the table view.
    self.tableView = [[UITableView alloc] initWithFrame:self.view.bounds style:UITableViewStylePlain];
    self.tableView.dataSource = self;
    self.tableView.delegate = self;
    [self.view addSubview:self.tableView];

    // Initialize the data source.
    self.dataSource = [[MyArrayDataSource alloc] initWithItems:@[@"Item 1", @"Item 2", @"Item 3"]];

    // Reload the table view.
    [self.tableView reloadData];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    // Return the number of rows in the section.
    return [self.dataSource numberOfItems];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    // Create a cell for the row.
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"Cell" forIndexPath:indexPath];

    // Configure the cell.
    cell.textLabel.text = [self.dataSource itemAtIndex:indexPath.row];

    // Return the cell.
    return cell;
}

@end

// Main function.
int main(int argc, char *argv[]) {
    @autoreleasepool {
        // Create an instance of the application delegate.
        AppDelegate *delegate = [[AppDelegate alloc] init];

        // Create an instance of the window.
        UIWindow *window = [[UIWindow alloc] initWithFrame:[UIScreen mainScreen].bounds];

        // Set the root view controller of the window.
        window.rootViewController = [[MyViewController alloc] init];

        // Make the window visible.
        [window makeKeyAndVisible];

        // Run the application.
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([delegate class]));
    }
}
```

Explanation:

This code is an implementation of a simple table view application in Objective-C.

1. **Data Source Protocol:**
   - We define a protocol called `MyDataSource` that declares two methods: `numberOfItems` and `itemAtIndex`. This protocol defines the interface for a data source that can provide items to a table view.

2. **Data Source Implementation:**
   - We implement a data source class called `MyArrayDataSource` that conforms to the `MyDataSource` protocol. This class provides an array of strings as the data source.

3. **View Controller:**
   - We create a view controller class called `MyViewController` that conforms to the `UITableViewDataSource` and `UITableViewDelegate` protocols. This class is responsible for managing the table view and presenting the data provided by the data source.

4. **View Controller Methods:**
   - In the `viewDidLoad` method, we initialize the table view, data source, and add the table view to the view controller's view.
   - In the `tableView:numberOfRowsInSection:` method, we return the number of items in the data source.
   - In the `tableView:cellForRowAtIndexPath:` method, we create a cell for each row in the table view and configure it with the data from the data source.

5. **Main Function:**
   - In the `main` function, we create an instance of the application delegate, create a window, set the root view controller of the window to an instance of `MyViewController`, and make the window visible.
   - We then call `UIApplicationMain()` to run the application.

This code demonstrates how to create a simple table view application in Objective-C, including the implementation of a data source and a view controller that manages the table view and displays the data provided by the data source.