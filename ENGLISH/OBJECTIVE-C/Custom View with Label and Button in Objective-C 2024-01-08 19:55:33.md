```objective-c
// Import necessary frameworks
#import <UIKit/UIKit.h>

// Define a custom view class
@interface CustomView : UIView

// Declare properties
@property (nonatomic, strong) UILabel *label;
@property (nonatomic, strong) UIButton *button;

// Initialize the view
- (instancetype)initWithFrame:(CGRect)frame;

// Handle button tap
- (void)buttonTapped:(UIButton *)sender;

@end

// Implement the custom view class
@implementation CustomView

// Initialize the view
- (instancetype)initWithFrame:(CGRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        
        // Create a label
        self.label = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height / 2)];
        self.label.text = @"Hello, World!";
        self.label.textAlignment = NSTextAlignmentCenter;
        [self addSubview:self.label];
        
        // Create a button
        self.button = [[UIButton alloc] initWithFrame:CGRectMake(0, frame.size.height / 2, frame.size.width, frame.size.height / 2)];
        [self.button setTitle:@"Tap Me!" forState:UIControlStateNormal];
        [self.button addTarget:self action:@selector(buttonTapped:) forControlEvents:UIControlEventTouchUpInside];
        [self addSubview:self.button];
    }
    return self;
}

// Handle button tap
- (void)buttonTapped:(UIButton *)sender {
    
    // Change the label text
    self.label.text = @"Button Tapped!";
    
    // Animate the label
    [UIView animateWithDuration:0.5 animations:^{
        self.label.transform = CGAffineTransformMakeScale(1.5, 1.5);
    } completion:^(BOOL finished) {
        [UIView animateWithDuration:0.5 animations:^{
            self.label.transform = CGAffineTransformIdentity;
        }];
    }];
}

@end

// Define a custom view controller class
@interface CustomViewController : UIViewController

// Declare properties
@property (nonatomic, strong) CustomView *customView;

// Initialize the view controller
- (instancetype)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil;

@end

// Implement the custom view controller class
@implementation CustomViewController

// Initialize the view controller
- (instancetype)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        
        // Create a custom view
        self.customView = [[CustomView alloc] initWithFrame:self.view.bounds];
        [self.view addSubview:self.customView];
    }
    return self;
}

@end

// Define the main application delegate class
@interface AppDelegate : UIResponder <UIApplicationDelegate>

// Declare properties
@property (nonatomic, strong) UIWindow *window;

// Initialize the application delegate
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions;

@end

// Implement the main application delegate class
@implementation AppDelegate

// Initialize the application delegate
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    
    // Create a window
    self.window = [[UIWindow alloc] initWithFrame:[UIScreen mainScreen].bounds];
    
    // Create a root view controller
    CustomViewController *rootViewController = [[CustomViewController alloc] init];
    
    // Set the root view controller
    self.window.rootViewController = rootViewController;
    
    // Make the window visible
    [self.window makeKeyAndVisible];
    
    return YES;
}

@end

// Define the main function
int main(int argc, char * argv[]) {
    
    // Create an application delegate
    AppDelegate *delegate = [[AppDelegate alloc] init];
    
    // Create an application
    UIApplication *application = [UIApplication sharedApplication];
    
    // Set the delegate
    application.delegate = delegate;
    
    // Run the application
    return [application run];
}
```

This code creates a custom view with a label and a button, and a custom view controller that displays the custom view. When the button is tapped, the label text changes and the label animates. The code is well-commented and follows best practices for Objective-C development.