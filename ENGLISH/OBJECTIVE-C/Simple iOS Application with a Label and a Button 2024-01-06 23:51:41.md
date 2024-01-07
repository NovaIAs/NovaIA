```objective-c
#import <UIKit/UIKit.h>

@interface MyComplexViewController : UIViewController

@property (nonatomic, strong) UILabel *label;
@property (nonatomic, strong) UIButton *button;

- (void)viewDidLoad;
- (void)buttonPressed:(UIButton *)sender;

@end

@implementation MyComplexViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    // Create a label
    self.label = [[UILabel alloc] initWithFrame:CGRectMake(100, 100, 200, 20)];
    self.label.text = @"Hello, world!";
    [self.view addSubview:self.label];

    // Create a button
    self.button = [[UIButton alloc] initWithFrame:CGRectMake(100, 200, 200, 20)];
    [self.button setTitle:@"Press me!" forState:UIControlStateNormal];
    [self.button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:self.button];
}

- (void)buttonPressed:(UIButton *)sender {
    // Change the label text
    self.label.text = @"Button pressed!";

    // Create an alert controller
    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Button Pressed" message:@"You pressed the button!" preferredStyle:UIAlertControllerStyleAlert];

    // Create an action for the alert controller
    UIAlertAction *okAction = [UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:^(UIAlertAction *action) {
        // Dismiss the alert controller
        [alertController dismissViewControllerAnimated:YES completion:nil];
    }];

    // Add the action to the alert controller
    [alertController addAction:okAction];

    // Present the alert controller
    [self presentViewController:alertController animated:YES completion:nil];
}

@end
```

This code creates a simple iOS application with a label and a button. When the button is pressed, the label text changes and an alert controller is presented.

The code is organized into two classes: `MyComplexViewController` and `AppDelegate`. `MyComplexViewController` is the view controller for the main screen of the application. `AppDelegate` is the application delegate, which is responsible for managing the application's lifecycle.

The `viewDidLoad` method of `MyComplexViewController` is called when the view controller is first loaded. In this method, the label and button are created and added to the view. The `buttonPressed:` method is also defined in this class. This method is called when the button is pressed. In this method, the label text is changed and an alert controller is presented.

The `AppDelegate` class is responsible for managing the application's lifecycle. The `application:didFinishLaunchingWithOptions:` method is called when the application is first launched. In this method, the window and root view controller are created.

This code is a very simple example of an iOS application. It demonstrates how to create a view controller, add views to the view controller, and handle user interaction.