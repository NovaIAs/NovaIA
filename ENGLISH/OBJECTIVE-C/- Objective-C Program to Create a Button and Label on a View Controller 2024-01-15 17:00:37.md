```objective-c
#import <UIKit/UIKit.h>

@interface MyViewController : UIViewController
@property (nonatomic, strong) UIButton *button;
@property (nonatomic, strong) UILabel *label;
@end

@implementation MyViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Create a button
    self.button = [[UIButton alloc] initWithFrame:CGRectMake(100, 100, 100, 50)];
    [self.button setTitle:@"Tap me!" forState:UIControlStateNormal];
    [self.button setTitleColor:[UIColor blueColor] forState:UIControlStateNormal];
    [self.button addTarget:self action:@selector(buttonTapped:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:self.button];
    
    // Create a label
    self.label = [[UILabel alloc] initWithFrame:CGRectMake(100, 200, 200, 50)];
    self.label.text = @"Hello, world!";
    self.label.textColor = [UIColor redColor];
    [self.view addSubview:self.label];
}

- (void)buttonTapped:(UIButton *)sender {
    self.label.text = @"Button tapped!";
}

@end

```

This is a simple Objective-C program that creates a button and a label on a view controller. When the button is tapped, the text of the label changes to "Button tapped!".

Here is a breakdown of the code:

* `#import <UIKit/UIKit.h>`: This line imports the UIKit framework, which is necessary for creating user interface elements.
* `@interface MyViewController : UIViewController`: This line declares a new class called `MyViewController`, which is a subclass of `UIViewController`. UIViewController is the base class for all view controllers in iOS.
* `@property (nonatomic, strong) UIButton *button;`: This line declares a property called `button` of type `UIButton`. The `nonatomic` attribute means that the property can be accessed from multiple threads at the same time. The `strong` attribute means that the property will be retained by the view controller, so it will not be deallocated until the view controller is deallocated.
* `@property (nonatomic, strong) UILabel *label;`: This line declares a property called `label` of type `UILabel`. The `nonatomic` and `strong` attributes have the same meaning as they do for the `button` property.
* `- (void)viewDidLoad { ... }`: This method is called when the view controller is first loaded. It is used to create the user interface elements and set up any initial state.
* `self.button = [[UIButton alloc] initWithFrame:CGRectMake(100, 100, 100, 50)];`: This line creates a new button with a frame of 100x100 pixels, starting at position (100, 100).
* `[self.button setTitle:@"Tap me!" forState:UIControlStateNormal];`: This line sets the title of the button to "Tap me!" for the normal state.
* `[self.button setTitleColor:[UIColor blueColor] forState:UIControlStateNormal];`: This line sets the title color of the button to blue for the normal state.
* `[self.button addTarget:self action:@selector(buttonTapped:) forControlEvents:UIControlEventTouchUpInside];`: This line adds a target to the button, which means that the `buttonTapped:` method will be called when the button is tapped.
* `[self.view addSubview:self.button];`: This line adds the button to the view of the view controller.
* `self.label = [[UILabel alloc] initWithFrame:CGRectMake(100, 200, 200, 50)];`: This line creates a new label with a frame of 200x50 pixels, starting at position (100, 200).
* `self.label.text = @"Hello, world!";`: This line sets the text of the label to "Hello, world!".
* `self.label.textColor = [UIColor redColor];`: This line sets the text color of the label to red.
* `[self.view addSubview:self.label];`: This line adds the label to the view of the view controller.
* `- (void)buttonTapped:(UIButton *)sender { ... }`: This method is called when the button is tapped. It sets the text of the label to "Button tapped!".