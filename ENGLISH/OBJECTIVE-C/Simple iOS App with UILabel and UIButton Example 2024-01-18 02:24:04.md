```objective-c
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.
    
    // Create a UILabel object
    UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(100, 100, 200, 20)];
    
    // Set the text of the UILabel object
    label.text = @"Hello, World!";
    
    //Set Font
    label.font = [UIFont fontWithName:@"Helvetica" size:20];
    
    // Set the color of the UILabel object
    label.textColor = [UIColor redColor];
    
    // Add the UILabel object to the view
    [self.view addSubview:label];
    
    // Create a UIButton object
    UIButton *button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    
    // Set the frame of the UIButton object
    button.frame = CGRectMake(100, 200, 100, 50);
    
    // Set the title of the UIButton object
    [button setTitle:@"Click Me!" forState:UIControlStateNormal];
    
    // Add the UIButton object to the view
    [self.view addSubview:button];
    
    // Add a target to the UIButton object
    [button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
}

- (void)buttonPressed:(id)sender {
    // Do something when the button is pressed
    
    // Create an UIAlertView object
    UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Button Pressed" message:@"You pressed the button!" delegate:nil cancelButtonTitle:@"OK" otherButtonTitles:nil];
    
    // Show the UIAlertView object
    [alert show];
}

@end
```

This code creates a simple iOS application with a UILabel object and a UIButton object. When the button is pressed, an UIAlertView object is shown.

The code is well-commented and easy to understand. It uses the UIKit framework to create the user interface objects. The code also uses the UIAlertView class to show an alert view when the button is pressed.

Here is a more detailed explanation of the code:

* The `viewDidLoad` method is called when the view is loaded. In this method, we create the UILabel object, the UIButton object, and add them to the view. We also add a target to the UIButton object so that when it is pressed, the `buttonPressed:` method is called.
* The `buttonPressed:` method is called when the button is pressed. In this method, we create an UIAlertView object and show it.

This code is a good example of how to use the UIKit framework to create a simple iOS application.