```objective-c
// Import necessary frameworks
#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>
#import "AppDelegate.h"

// Define a custom UITableViewCell subclass
@interface CustomCell : UITableViewCell

// Define properties for the cell's data
@property NSString *title;
@property NSString *subtitle;
@property UIImage *image;

// Define methods to initialize and configure the cell
- (instancetype)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier;
- (void)configureCellWithTitle:(NSString *)title subtitle:(NSString *)subtitle image:(UIImage *)image;

@end

// Implementation of the CustomCell class
@implementation CustomCell

- (instancetype)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier {
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        // Initialize the cell's subviews
        self.textLabel.font = [UIFont systemFontOfSize:16.0];
        self.detailTextLabel.font = [UIFont systemFontOfSize:14.0];
        self.imageView.contentMode = UIViewContentModeScaleAspectFit;
    }
    return self;
}

- (void)configureCellWithTitle:(NSString *)title subtitle:(NSString *)subtitle image:(UIImage *)image {
    self.title = title;
    self.subtitle = subtitle;
    self.image = image;

    // Update the cell's UI elements
    self.textLabel.text = title;
    self.detailTextLabel.text = subtitle;
    self.imageView.image = image;
}

@end

// Define the main ViewController class
@interface ViewController : UIViewController <UITableViewDataSource, UITableViewDelegate>

// Define properties for the table view and data source
@property UITableView *tableView;
@property NSArray *dataSource;

@end

// Implementation of the ViewController class
@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    // Initialize the table view and set its data source and delegate
    self.tableView = [[UITableView alloc] initWithFrame:self.view.bounds style:UITableViewStylePlain];
    self.tableView.dataSource = self;
    self.tableView.delegate = self;

    // Register the custom cell class with the table view
    [self.tableView registerClass:[CustomCell class] forCellReuseIdentifier:@"CustomCell"];

    // Create a data source array
    self.dataSource = @[@{@"title": @"Item 1", @"subtitle": @"Subtitle 1", @"image": [UIImage imageNamed:@"image1.png"]},
                        @{@"title": @"Item 2", @"subtitle": @"Subtitle 2", @"image": [UIImage imageNamed:@"image2.png"]},
                        @{@"title": @"Item 3", @"subtitle": @"Subtitle 3", @"image": [UIImage imageNamed:@"image3.png"]}];
}

#pragma mark - UITableViewDataSource methods

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.dataSource.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    // Dequeue a reusable cell
    CustomCell *cell = [tableView dequeueReusableCellWithIdentifier:@"CustomCell" forIndexPath:indexPath];

    // Get the data for the current row
    NSDictionary *data = self.dataSource[indexPath.row];

    // Configure the cell with the data
    [cell configureCellWithTitle:data[@"title"] subtitle:data[@"subtitle"] image:data[@"image"]];

    return cell;
}

#pragma mark - UITableViewDelegate methods

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    // Handle cell selection
    NSLog(@"Selected cell at index path: %@", indexPath);
}

@end

// Define the AppDelegate class
@interface AppDelegate : UIResponder <UIApplicationDelegate>

@property UIWindow *window;

@end

// Implementation of the AppDelegate class
@implementation AppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    // Create a window and set its root view controller
    self.window = [[UIWindow alloc] initWithFrame:[UIScreen mainScreen].bounds];
    self.window.rootViewController = [[UINavigationController alloc] initWithRootViewController:[[ViewController alloc] init]];
    [self.window makeKeyAndVisible];

    return YES;
}

@end
```

This code creates a simple iOS app with a table view that displays a list of items with titles, subtitles, and images. Here's an explanation of the code:

1. Custom Cell:
   - We define a custom UITableViewCell subclass called `CustomCell`.
   - It has properties to store the title, subtitle, and image data.
   - It provides methods to initialize and configure the cell with the data.

2. ViewController:
   - The `ViewController` class is the main view controller for the app.
   - It conforms to the `UITableViewDataSource` and `UITableViewDelegate` protocols.
   - It initializes the table view, sets its data source and delegate, and registers the custom cell class.
   - It creates a data source array containing dictionaries with item data.

3. UITableViewDataSource Methods:
   - `tableView:numberOfRowsInSection:` returns the number of rows in the table view based on the data source count.
   - `tableView:cellForRowAtIndexPath:` dequeues and configures a custom cell with data for the given index path.

4. UITableViewDelegate Method:
   - `tableView:didSelectRowAtIndexPath:` handles cell selection by printing the selected index path.

5. AppDelegate:
   - The `AppDelegate` class is responsible for setting up the application window and root view controller.

This code sets up a functional table view with custom cells that display item data. You can customize the data and appearance of the table view to suit your specific needs.