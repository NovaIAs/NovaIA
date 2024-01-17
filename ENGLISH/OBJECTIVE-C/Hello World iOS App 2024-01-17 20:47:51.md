```objective-c
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@property (nonatomic, strong) UILabel *label;
@property (nonatomic, strong) UIButton *button;

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Create a label
    self.label = [[UILabel alloc] initWithFrame:CGRectMake(100, 100, 200, 20)];
    self.label.text = @"Hello, world!";
    [self.view addSubview:self.label];
    
    // Create a button
    self.button = [[UIButton alloc] initWithFrame:CGRectMake(100, 150, 100, 50)];
    [self.button setTitle:@"Click me!" forState:UIControlStateNormal];
    [self.button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:self.button];
}

- (void)buttonPressed:(UIButton *)sender {
    // Change the label's text
    self.label.text = @"You clicked the button!";
}

@end
```

This code creates a simple iOS app with a label and a button. When the button is pressed, the label's text is changed.

Here is a breakdown of the code:

* The `ViewController` class is a subclass of `UIViewController`, which is the base class for all view controllers in UIKit.
* The `viewDidLoad` method is called when the view controller's view is loaded into memory. This is where we create the label and the button.
* The `label` property is a UILabel object, which is used to display text.
* The `button` property is a UIButton object, which is used to allow the user to interact with the app.
* The `buttonPressed:` method is called when the button is pressed. This method changes the label's text.

This code is a simple example of how to create a basic iOS app. You can use this code as a starting point for creating your own apps.