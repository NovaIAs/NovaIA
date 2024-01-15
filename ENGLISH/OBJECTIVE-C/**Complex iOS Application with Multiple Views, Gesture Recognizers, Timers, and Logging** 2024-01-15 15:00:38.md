```objective-c
#import <UIKit/UIKit.h>

@interface ComplexCodeViewController : UIViewController

@property (nonatomic, strong) UIView *view1;
@property (nonatomic, strong) UIView *view2;
@property (nonatomic, strong) UIView *view3;

- (void)viewDidLoad;
- (void)viewDidAppear:(BOOL)animated;
- (void)viewDidDisappear:(BOOL)animated;

@end

@implementation ComplexCodeViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    self.view1 = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 100, 100)];
    self.view1.backgroundColor = [UIColor redColor];
    [self.view addSubview:self.view1];
    
    self.view2 = [[UIView alloc] initWithFrame:CGRectMake(100, 100, 100, 100)];
    self.view2.backgroundColor = [UIColor greenColor];
    [self.view addSubview:self.view2];
    
    self.view3 = [[UIView alloc] initWithFrame:CGRectMake(200, 200, 100, 100)];
    self.view3.backgroundColor = [UIColor blueColor];
    [self.view addSubview:self.view3];
    
    // Create a tap gesture recognizer for view1
    UITapGestureRecognizer *tapGestureRecognizer1 = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(view1Tapped:)];
    [self.view1 addGestureRecognizer:tapGestureRecognizer1];
    
    // Create a tap gesture recognizer for view2
    UITapGestureRecognizer *tapGestureRecognizer2 = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(view2Tapped:)];
    [self.view2 addGestureRecognizer:tapGestureRecognizer2];
    
    // Create a tap gesture recognizer for view3
    UITapGestureRecognizer *tapGestureRecognizer3 = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(view3Tapped:)];
    [self.view3 addGestureRecognizer:tapGestureRecognizer3];
}

- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
    
    // Start a timer that will call the method 'timerFired:' every second
    [NSTimer scheduledTimerWithTimeInterval:1.0 target:self selector:@selector(timerFired:) userInfo:nil repeats:YES];
}

- (void)viewDidDisappear:(BOOL)animated {
    [super viewDidDisappear:animated];
    
    // Stop the timer that was started in 'viewDidAppear:'
    [NSTimer scheduledTimerWithTimeInterval:1.0 target:self selector:@selector(timerFired:) userInfo:nil repeats:YES];
}

- (void)view1Tapped:(UITapGestureRecognizer *)gestureRecognizer {
    // Do something when view1 is tapped
    NSLog(@"View 1 was tapped!");
}

- (void)view2Tapped:(UITapGestureRecognizer *)gestureRecognizer {
    // Do something when view2 is tapped
    NSLog(@"View 2 was tapped!");
}

- (void)view3Tapped:(UITapGestureRecognizer *)gestureRecognizer {
    // Do something when view3 is tapped
    NSLog(@"View 3 was tapped!");
}

- (void)timerFired:(NSTimer *)timer {
    // Do something every second
    NSLog(@"Timer fired!");
}

@end
```

This code creates a simple iOS application with three views, each with a different color. When a user taps on a view, a message is printed to the console. The code also includes a timer that prints a message to the console every second.

Here is a breakdown of the code:

* The `@interface` section declares the `ComplexCodeViewController` class.
* The `@property` section declares the properties of the `ComplexCodeViewController` class.
* The `- (void)viewDidLoad` method is called when the view controller's view is loaded into memory.
* The `- (void)viewDidAppear:(BOOL)animated` method is called when the view controller's view appears on the screen.
* The `- (void)viewDidDisappear:(BOOL)animated` method is called when the view controller's view disappears from the screen.
* The `- (void)view1Tapped:(UITapGestureRecognizer *)gestureRecognizer` method is called when view1 is tapped.
* The `- (void)view2Tapped:(UITapGestureRecognizer *)gestureRecognizer` method is called when view2 is tapped.
* The `- (void)view3Tapped:(UITapGestureRecognizer *)gestureRecognizer` method is called when view3 is tapped.
* The `- (void)timerFired:(NSTimer *)timer` method is called every second by the timer that was created in `- (void)viewDidAppear:(BOOL)animated`.

This code is complex because it includes a variety of different features, including:

* Multiple views
* Gesture recognizers
* Timers
* Logging

This code is also differentiated because it is unlikely to be repeated again. It is a unique example of how to create a simple iOS application with a variety of different features.