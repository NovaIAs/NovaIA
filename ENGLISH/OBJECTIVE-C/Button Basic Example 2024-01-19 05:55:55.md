```objective-c
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@property (nonatomic, strong) UIButton *button;

- (void)buttonPressed:(UIButton *)sender;

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Create a button
    self.button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    self.button.frame = CGRectMake(100, 100, 100, 50);
    [self.button setTitle:@"Press Me" forState:UIControlStateNormal];
    [self.button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:self.button];
}

- (void)buttonPressed:(UIButton *)sender {
    // When the button is pressed, show an alert
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Button Pressed" message:@"You pressed the button!" delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil];
    [alert show];
}

@end
```

This code creates a simple button in the center of the screen. When the button is pressed, an alert is shown.

The code is explained in detail below:

* The first line imports the `UIKit` framework, which is necessary for working with user interface elements.
* The `ViewController` class is defined as a subclass of `UIViewController`, which is the base class for all view controllers in iOS.
* The `button` property is declared as a strong reference to a `UIButton` object. This means that the button will be retained by the view controller, so it will not be deallocated until the view controller is deallocated.
* The `viewDidLoad` method is called when the view controller's view is loaded into memory. In this method, the button is created, configured, and added to the view.
* The `buttonPressed:` method is called when the button is pressed. This method creates an alert view, sets its title and message, and then shows the alert view.

This code is a good example of how to create a simple user interface in iOS. It uses the `UIKit` framework to create a button and an alert view. The code is also well-organized and easy to read.