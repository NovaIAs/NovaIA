```objective-c
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@property (nonatomic, strong) UIButton *button;

- (void)viewDidLoad;

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    self.view.backgroundColor = [UIColor whiteColor];

    self.button = [UIButton buttonWithType:UIButtonTypeSystem];
    self.button.frame = CGRectMake(100, 100, 100, 50);
    self.button.setTitle:@"Press me", forState:UIControlStateNormal);
    [self.button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:self.button];
}

- (void)buttonPressed:(UIButton *)sender {
    NSLog(@"Button pressed!");

    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Button Pressed" message:@"You pressed the button!" preferredStyle:UIAlertControllerStyleAlert];
    UIAlertAction *okAction = [UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:nil];
    [alertController addAction:okAction];
    [self presentViewController:alertController animated:YES completion:nil];
}

@end
```

This code creates a simple iOS app with a button. When the button is pressed, an alert dialog is displayed.

Here's a breakdown of the code:

1. Import the necessary Cocoa frameworks:

    ```objective-c
    #import <Foundation/Foundation.h>
    #import <UIKit/UIKit.h>
    ```

2. Declare the `ViewController` class:

    ```objective-c
    @interface ViewController : UIViewController
    @end
    ```

3. Implement the `viewDidLoad` method in the `ViewController` class. This method is called when the view controller's view is loaded into memory:

    ```objective-c
    - (void)viewDidLoad {
        // ...
    }
    ```

4. Create a `UIButton` instance and configure its properties:

    ```objective-c
    self.button = [UIButton buttonWithType:UIButtonTypeSystem];
    self.button.frame = CGRectMake(100, 100, 100, 50);
    self.button.setTitle(@"Press me", forState:UIControlStateNormal);
    ```

5. Add a target to the button so that it calls the `buttonPressed:` method when it is tapped:

    ```objective-c
    [self.button addTarget:self action:@selector(buttonPressed:) forControlEvents:UIControlEventTouchUpInside];
    ```

6. Add the button to the view controller's view:

    ```objective-c
    [self.view addSubview:self.button];
    ```

7. Define the `buttonPressed:` method, which is called when the button is tapped:

    ```objective-c
    - (void)buttonPressed:(UIButton *)sender {
        // ...
    }
    ```

8. In the `buttonPressed:` method, create an `UIAlertController` instance and configure its properties:

    ```objective-c
    UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Button Pressed" message:@"You pressed the button!" preferredStyle:UIAlertControllerStyleAlert];
    ```

9. Create an `UIAlertAction` instance and configure its properties:

    ```objective-c
    UIAlertAction *okAction = [UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:nil];
    ```

10. Add the action to the alert controller:

    ```objective-c
    [alertController addAction:okAction];
    ```

11. Present the alert controller:

    ```objective-c
    [self presentViewController:alertController animated:YES completion:nil];
    ```