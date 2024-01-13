```objective-c
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@property (nonatomic, strong) IBOutlet UIButton *button;
@property (nonatomic, strong) IBOutlet UILabel *label;

- (IBAction)buttonPressed:(id)sender;

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Set the initial text of the label
    self.label.text = @"Hello, world!";
    
    // Add a target to the button
    [self.button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
}

- (IBAction)buttonPressed:(id)sender {
    // Change the text of the label
    self.label.text = @"Button pressed!";
}

@end
```

This code creates a simple iOS app with a button and a label. When the button is pressed, the text of the label is changed to "Button pressed!".

The code is explained in more detail below:

* The `@import` statement imports the UIKit framework, which provides the classes and functions needed to create iOS apps.
* The `@interface` statement declares the `ViewController` class. The `@property` statements declare the instance variables of the class. The `@end` statement marks the end of the class declaration.
* The `@implementation` statement implements the methods of the `ViewController` class. The `viewDidLoad` method is called when the view controller is loaded into memory. The `buttonPressed:` method is called when the button is pressed.
* The `viewDidLoad` method sets the initial text of the label and adds a target to the button. The `buttonPressed:` method changes the text of the label when the button is pressed.

This code is a simple example of how to create an iOS app. It can be used as a starting point for more complex apps.