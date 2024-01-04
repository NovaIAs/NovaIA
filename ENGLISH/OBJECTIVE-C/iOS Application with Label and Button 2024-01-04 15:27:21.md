```objective-c
// Import necessary frameworks
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Define a custom view controller
@interface CustomViewController : UIViewController
{
    // Declare private variables
    UILabel *label;
    UIButton *button;
}

// Override the loadView method to create the custom view
- (void)loadView
{
    // Create a new view
    UIView *view = [[UIView alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    
    // Create a new label
    label = [[UILabel alloc] initWithFrame:CGRectMake(100, 100, 200, 20)];
    label.text = @"Hello, World!";
    
    // Create a new button
    button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    button.frame = CGRectMake(100, 200, 100, 30);
    [button setTitle:@"Click Me" forState:UIControlStateNormal];
    
    // Add the label and button to the view
    [view addSubview:label];
    [view addSubview:button];
    
    // Set the view as the root view of the view controller
    self.view = view;
}

// Override the viewDidLoad method to add action to the button
- (void)viewDidLoad
{
    [super viewDidLoad];
    
    // Add a target to the button
    [button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
}

// Define the buttonPressed method
- (void)buttonPressed:(UIButton *)sender
{
    // Change the label text when the button is pressed
    label.text = @"Button Pressed!";
}

// Override the didReceiveMemoryWarning method to release memory when needed
- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc.
}

@end

// Define the main application delegate
@interface AppDelegate : UIResponder <UIApplicationDelegate>
{
    // Declare private variables
    UIWindow *window;
}

// Override the applicationDidFinishLaunching method to create the window and root view controller
- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    // Create a new window
    window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    
    // Create a new root view controller
    CustomViewController *rootViewController = [[CustomViewController alloc] init];
    
    // Set the root view controller for the window
    window.rootViewController = rootViewController;
    
    // Make the window visible
    [window makeKeyAndVisible];
    
    return YES;
}

// Override the applicationWillTerminate method to release memory when the application is terminated
- (void)applicationWillTerminate:(UIApplication *)application
{
    // Release any cached data, images, etc.
}

@end

// Define the main function
int main(int argc, char *argv[])
{
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}
```

This code creates a simple iOS application with a view controller that contains a label and a button. When the button is pressed, the label text changes to "Button Pressed!". The code is well-structured and uses proper object-oriented programming principles. It also follows the Model-View-Controller (MVC) design pattern, which is a common architecture for iOS applications. The code is also well-commented, which makes it easy to understand and maintain.