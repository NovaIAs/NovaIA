```objective-c
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@interface ComplexViewController : UIViewController

@property (nonatomic, strong) UIButton *button;
@property (nonatomic, strong) UILabel *label;

@end

@implementation ComplexViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Create a button
    self.button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    self.button.frame = CGRectMake(100, 100, 100, 50);
    [self.button setTitle:@"Press Me" forState:UIControlStateNormal];
    [self.button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:self.button];
    
    // Create a label
    self.label = [[UILabel alloc] initWithFrame:CGRectMake(100, 200, 200, 50)];
    self.label.text = @"Hello, World!";
    [self.view addSubview:self.label];
}

- (void)buttonPressed:(UIButton *)sender {
    // Change the label text
    self.label.text = @"Button Pressed!";
    
    // Create a random color
    CGFloat red = arc4random() % 256 / 255.0;
    CGFloat green = arc4random() % 256 / 255.0;
    CGFloat blue = arc4random() % 256 / 255.0;
    UIColor *color = [UIColor colorWithRed:red green:green blue:blue alpha:1.0];
    
    // Change the background color of the view
    self.view.backgroundColor = color;
}

@end

int main(int argc, char * argv[]) {
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([ComplexViewController class]));
    }
}
```

This code creates a simple iOS application with a button and a label. When the button is pressed, the label text changes and the background color of the view changes to a random color. Here's a detailed explanation of the code:

**Objective-C Class Definition:**

```objective-c
@interface ComplexViewController : UIViewController
```

This line declares a new Objective-C class named `ComplexViewController` that inherits from `UIViewController`. This is the main controller class for the view controller that will be displayed in the application.

**Properties:**

```objective-c
@property (nonatomic, strong) UIButton *button;
@property (nonatomic, strong) UILabel *label;
```

These lines declare two properties of the `ComplexViewController` class:

- `button`: This property will hold a reference to a `UIButton` object, which is the button that will be displayed in the view controller.
- `label`: This property will hold a reference to a `UILabel` object, which is the label that will display text in the view controller.

**viewDidLoad Method:**

```objective-c
- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Create a button
    self.button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    self.button.frame = CGRectMake(100, 100, 100, 50);
    [self.button setTitle:@"Press Me" forState:UIControlStateNormal];
    [self.button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:self.button];
    
    // Create a label
    self.label = [[UILabel alloc] initWithFrame:CGRectMake(100, 200, 200, 50)];
    self.label.text = @"Hello, World!";
    [self.view addSubview:self.label];
}
```

The `viewDidLoad` method is called when the view controller's view is loaded into memory. In this method:

- A `UIButton` object is created and configured. The button's frame is set, its title is set to "Press Me", and a target-action pair is added to the button so that when the button is pressed, the `buttonPressed:` method will be called.
- A `UILabel` object is created and configured. The label's frame is set and its text is set to "Hello, World!".
- Both the button and the label are added as subviews to the view controller's view.

**buttonPressed Method:**

```objective-c
- (void)buttonPressed:(UIButton *)sender {
    // Change the label text
    self.label.text = @"Button Pressed!";
    
    // Create a random color
    CGFloat red = arc4random() % 256 / 255.0;
    CGFloat green = arc4random() % 256 / 255.0;
    CGFloat blue = arc4random() % 256 / 255.0;
    UIColor *color = [UIColor colorWithRed:red green:green blue:blue alpha:1.0];
    
    // Change the background color of the view
    self.view.backgroundColor = color;
}
```

The `buttonPressed:` method is called when the button is pressed. In this method:

- The label's text is changed to "Button Pressed!".
- A random color is generated using the `arc4random` function and a `UIColor` object is created with that color.
- The background color of the view controller's view is set to the random color.

**main Function:**

```objective-c
int main(int argc, char * argv[]) {
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([ComplexViewController class]));
    }
}
```

This is the entry point of the application. It sets up the UIApplication, the main application object, and then calls `UIApplicationMain`. The `UIApplicationMain` function starts the event loop and manages the application's life cycle.