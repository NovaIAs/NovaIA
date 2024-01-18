**Code Objective-C:**

```objc
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Creating the main view
    UIView *mainView = [[UIView alloc] initWithFrame:self.view.bounds];
    mainView.backgroundColor = [UIColor whiteColor];
    [self.view addSubview:mainView];
    
    // Creating a label
    UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(100, 100, 200, 50)];
    label.text = @"Hello, world!";
    label.textAlignment = NSTextAlignmentCenter;
    label.textColor = [UIColor blackColor];
    [mainView addSubview:label];
    
    // Creating a button
    UIButton *button = [UIButton buttonWithType:UIButtonTypeSystem];
    button.frame = CGRectMake(100, 200, 200, 50);
    [button setTitle:@"Press me!" forState:UIControlStateNormal];
    [button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [mainView addSubview:button];
    
    // Creating a text field
    UITextField *textField = [[UITextField alloc] initWithFrame:CGRectMake(100, 300, 200, 50)];
    textField.placeholder = @"Enter some text";
    textField.borderStyle = UITextBorderStyleRoundedRect;
    [mainView addSubview:textField];
    
    // Creating a table view
    UITableView *tableView = [[UITableView alloc] initWithFrame:CGRectMake(100, 400, 200, 300)];
    tableView.dataSource = self;
    tableView.delegate = self;
    [mainView addSubview:tableView];
    
    // Creating an array of data for the table view
    self.tableData = @[@"Item 1", @"Item 2", @"Item 3", @"Item 4", @"Item 5"];
}

- (void)buttonPressed:(UIButton *)sender {
    // Handle button press event
    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Button Pressed!" message:@"You pressed the button!" preferredStyle:UIAlertControllerStyleAlert];
    [alertController addAction:[UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:nil]];
    [self presentViewController:alertController animated:YES completion:nil];
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.tableData.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"cell"];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"cell"];
    }
    cell.textLabel.text = self.tableData[indexPath.row];
    return cell;
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    // Handle table view cell selection event
    NSString *selectedText = self.tableData[indexPath.row];
    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Cell Selected!" message:[NSString stringWithFormat:@"You selected %@", selectedText] preferredStyle:UIAlertControllerStyleAlert];
    [alertController addAction:[UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:nil]];
    [self presentViewController:alertController animated:YES completion:nil];
}

@end
```

**Explanation:**

This Objective-C code creates a simple iOS application with a user interface consisting of a label, a button, a text field, and a table view. When the button is pressed, an alert dialog is displayed with a message indicating that the button was pressed. The table view displays a list of items, and when an item is selected, an alert dialog is displayed with a message indicating which item was selected.

Here is a breakdown of the code:

1. **Import the UIKit Framework:**

```objc
#import <UIKit/UIKit.h>
```

This line imports the UIKit framework, which provides the classes and protocols needed to develop iOS applications.

2. **Define the ViewController Class:**

```objc
@interface ViewController : UIViewController

@end

@implementation ViewController
```

This defines the ViewController class, which is a subclass of the UIViewController class. The ViewController class will be responsible for managing the user interface and handling user interactions.

3. **Create the Main View:**

```objc
- (void)viewDidLoad {
    [super viewDidLoad];

    UIView *mainView = [[UIView alloc] initWithFrame:self.view.bounds];
    mainView.backgroundColor = [UIColor whiteColor];
    [self.view addSubview:mainView];
}
```

In the viewDidLoad method, we create the main view of the application. The main view is a UIView object that fills the entire screen. We set the background color of the main view to white and add it as a subview of the view controller's view.

4. **Create the Label:**

```objc
UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(100, 100, 200, 50)];
label.text = @"Hello, world!";
label.textAlignment = NSTextAlignmentCenter;
label.textColor = [UIColor blackColor];
[mainView addSubview:label];
```

We create a UILabel object and set its frame, text, text alignment, and text color. We then add the label as a subview of the main view.

5. **Create the Button:**

```objc
UIButton *button = [UIButton buttonWithType:UIButtonTypeSystem];
button.frame = CGRectMake(100, 200, 200, 50);
[button setTitle:@"Press me!" forState:UIControlStateNormal];
[button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
[mainView addSubview:button];
```

We create a UIButton object and set its frame, title, and target-action for the touchUpInside event. We then add the button as a subview of the main view.

6. **Create the Text Field:**

```objc
UITextField *textField = [[UITextField alloc] initWithFrame:CGRectMake(100, 300, 200, 50)];
textField.placeholder = @"Enter some text";
textField.borderStyle = UITextBorderStyleRoundedRect;
[mainView addSubview:textField];
```

We create a UITextField object and set its frame, placeholder text, and border style. We then add the text field as a subview of the main view.

7. **Create the Table View:**

```objc
UITableView *tableView = [[UITableView alloc] initWithFrame:CGRectMake(100, 400, 200, 300)];
tableView.dataSource = self;
tableView.delegate = self;
[mainView addSubview:tableView];
```

We create a UITableView object and set its frame, data source, and delegate. We then add the table view as a subview of the main view.

8. **Create the Table View Data:**

```objc
self.tableData = @[@"Item 1", @"Item 2", @"Item 3", @"Item 4", @"Item 5"];
```

We create an array of strings to be displayed in the table view.

9. **Implement the Button Pressed Event Handler:**

```objc
- (void)buttonPressed:(UIButton *)sender {
    // Handle button press event
    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Button Pressed!" message:@"You pressed the button!" preferredStyle:UIAlertControllerStyleAlert];
    [alertController addAction:[UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:nil]];
    [self presentViewController:alertController animated:YES completion:nil];
}
```

We implement the buttonPressed method to handle the button press event. When the button is pressed, we display an alert dialog with a message indicating that the button was pressed.

10. **Implement the Table View Data Source and Delegate Methods:**

```objc
- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.tableData.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"cell"];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"cell"];
    }
    cell.textLabel.text = self.tableData[indexPath.row];
    return cell;
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    // Handle table view cell selection event
    NSString *selectedText = self.tableData[indexPath.row];
    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Cell Selected!" message:[NSString stringWithFormat:@"You selected %@", selectedText] preferredStyle:UIAlertControllerStyleAlert];
    [alertController addAction:[UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:nil]];
    [self presentViewController:alertController animated:YES completion:nil];
}
```

We implement the required table view data source and delegate methods to provide data to the table view and handle user interactions with the table view cells.

This code provides a basic understanding of how to create a simple iOS application with a user interface using Objective-C.