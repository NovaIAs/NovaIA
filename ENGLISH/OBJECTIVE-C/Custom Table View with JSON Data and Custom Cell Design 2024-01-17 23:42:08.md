```objective-c
// Import necessary headers
#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>

// Define the interface for a custom UITableViewCell
@interface CustomTableViewCell : UITableViewCell

// Outlets for the cell's subviews
@property (nonatomic, strong) UILabel *titleLabel;
@property (nonatomic, strong) UILabel *subtitleLabel;
@property (nonatomic, strong) UIImageView *thumbnailImageView;

@end

// Implement the CustomTableViewCell class
@implementation CustomTableViewCell

// Initialize the cell's subviews
- (instancetype)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier {
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        // Create and configure the title label
        self.titleLabel = [[UILabel alloc] init];
        self.titleLabel.font = [UIFont boldSystemFontOfSize:16.0];
        [self.contentView addSubview:self.titleLabel];

        // Create and configure the subtitle label
        self.subtitleLabel = [[UILabel alloc] init];
        self.subtitleLabel.font = [UIFont systemFontOfSize:14.0];
        [self.contentView addSubview:self.subtitleLabel];

        // Create and configure the thumbnail image view
        self.thumbnailImageView = [[UIImageView alloc] init];
        self.thumbnailImageView.contentMode = UIViewContentModeScaleAspectFit;
        [self.contentView addSubview:self.thumbnailImageView];
    }
    return self;
}

// Layout the cell's subviews
- (void)layoutSubviews {
    [super layoutSubviews];

    // Set the frames of the subviews
    self.titleLabel.frame = CGRectMake(10.0, 10.0, self.contentView.frame.size.width - 100.0, 20.0);
    self.subtitleLabel.frame = CGRectMake(10.0, 30.0, self.contentView.frame.size.width - 100.0, 20.0);
    self.thumbnailImageView.frame = CGRectMake(self.contentView.frame.size.width - 90.0, 10.0, 80.0, 80.0);
}

@end

// Define the interface for a custom UITableViewDataSource
@interface CustomTableViewDataSource : NSObject <UITableViewDataSource>

// Array of data to display in the table view
@property (nonatomic, strong) NSArray *data;

@end

// Implement the CustomTableViewDataSource class
@implementation CustomTableViewDataSource

// Return the number of rows in the table view
- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.data.count;
}

// Return a cell for a given row in the table view
- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    // Dequeue a reusable cell if available, otherwise create a new one
    CustomTableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"CustomCell" forIndexPath:indexPath];

    // Configure the cell with data
    NSDictionary *item = self.data[indexPath.row];
    cell.titleLabel.text = item[@"title"];
    cell.subtitleLabel.text = item[@"subtitle"];
    cell.thumbnailImageView.image = [UIImage imageNamed:item[@"image"]];

    return cell;
}

@end

// Define the interface for a custom UITableViewController
@interface CustomTableViewController : UITableViewController

// Data source for the table view
@property (nonatomic, strong) CustomTableViewDataSource *dataSource;

@end

// Implement the CustomTableViewController class
@implementation CustomTableViewController

// Initialize the table view controller
- (instancetype)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Create the data source
        self.dataSource = [[CustomTableViewDataSource alloc] init];

        // Load the data from a JSON file
        NSString *filePath = [[NSBundle mainBundle] pathForResource:@"data" ofType:@"json"];
        NSData *data = [NSData dataWithContentsOfFile:filePath];
        NSArray *json = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingMutableContainers error:nil];

        // Set the data source's data
        self.dataSource.data = json;
    }
    return self;
}

// Load the table view's data
- (void)viewDidLoad {
    [super viewDidLoad];

    // Set the table view's data source and delegate
    self.tableView.dataSource = self.dataSource;
    self.tableView.delegate = self;

    // Register the custom cell class with the table view
    [self.tableView registerClass:[CustomTableViewCell class] forCellReuseIdentifier:@"CustomCell"];
}

@end

// Define the main function
int main(int argc, char *argv[]) {
    @autoreleasepool {
        // Create a new UIApplication instance
        UIApplication *application = [UIApplication sharedApplication];

        // Create a new UIWindow instance
        UIWindow *window = [[UIWindow alloc] initWithFrame:[UIScreen mainScreen].bounds];

        // Create a new CustomTableViewController instance
        CustomTableViewController *controller = [[CustomTableViewController alloc] init];

        // Set the window's root view controller
        window.rootViewController = controller;

        // Make the window visible
        [window makeKeyAndVisible];

        // Run the UIApplication main loop
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}
```

This code creates a custom UITableViewCell class, a custom UITableViewDataSource class, and a custom UITableViewController class. It then loads data from a JSON file and displays it in a table view. The code is well-commented and easy to understand.