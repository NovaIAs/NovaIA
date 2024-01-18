**Objective-C Code Snippet:**

```objective-c
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController <UITableViewDataSource, UITableViewDelegate>

@property (strong, nonatomic) NSArray *tableData;
@property (strong, nonatomic) UITableView *tableView;

- (void)viewDidLoad;
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section;
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath;

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    self.tableData = @[@"One", @"Two", @"Three", @"Four", @"Five"];
    
    self.tableView = [[UITableView alloc] initWithFrame:self.view.frame style:UITableViewStylePlain];
    self.tableView.dataSource = self;
    self.tableView.delegate = self;
    [self.view addSubview:self.tableView];
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.tableData.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"Cell"];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"Cell"];
    }
    
    cell.textLabel.text = self.tableData[indexPath.row];
    return cell;
}

@end
```

**Explanation:**

This code snippet demonstrates the creation of a simple UITableView in Objective-C. Here's a breakdown:

1. **Importing UIKit**: `#import <UIKit/UIKit.h>`: This line imports the UIKit framework, which contains essential classes and protocols for building user interfaces in iOS.

2. **ViewController Class**: `@interface ViewController : UIViewController`: This declares the ViewController class, which inherits from UIViewController, the base class for UIViewController objects.

3. **Property Declarations**: `@property (strong, nonatomic) NSArray *tableData;` and `@property (strong, nonatomic) UITableView *tableView;`: These lines declare two properties: `tableData` to hold the data for the table view and `tableView` to represent the table view object.

4. **Implementation of viewDidLoad**: `- (void)viewDidLoad;`: This method is called when the view controller is loaded into memory. It initializes the `tableData` array with some sample data and creates a table view object (`UITableView`) using the device's frame and a plain style. The table view is then added as a subview to the view controller's view.

5. **UITableViewDataSource and UITableViewDelegate Methods**: The ViewController class conforms to the UITableViewDataSource and UITableViewDelegate protocols, which allow it to provide data and handle events related to the table view.

6. **Implementation of tableView:numberOfRowsInSection:**: This method returns the number of rows to display in the table view, which is the count of the `tableData` array.

7. **Implementation of tableView:cellForRowAtIndexPath:**: This method is called for each row in the table view and is responsible for creating and populating the table view cells. It dequeues a reusable cell or creates a new one if needed, sets its text label with the corresponding data from `tableData`, and returns the cell.

Overall, this code showcases the creation of a basic table view in Objective-C, demonstrating how to set up the data source, delegate methods, and cell configuration for the table view.