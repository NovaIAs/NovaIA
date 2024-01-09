```objective-c
// Import necessary frameworks
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Define a custom UIView subclass
@interface CustomView : UIView
@end

// Implement the CustomView class
@implementation CustomView

// Override the drawRect method to draw custom content
- (void)drawRect:(CGRect)rect {
    // Create a graphics context
    CGContextRef context = UIGraphicsGetCurrentContext();

    // Set the fill color to red
    CGContextSetFillColorWithColor(context, [UIColor redColor].CGColor);

    // Fill a rectangle with the red color
    CGContextFillRect(context, CGRectMake(0, 0, 100, 100));

    // Set the stroke color to blue
    CGContextSetStrokeColorWithColor(context, [UIColor blueColor].CGColor);

    // Stroke a rectangle with the blue color
    CGContextStrokeRect(context, CGRectMake(100, 100, 100, 100));

    // Create a path for a bezier curve
    CGMutablePathRef path = CGPathCreateMutable();
    CGPathMoveToPoint(path, NULL, 200, 200);
    CGPathAddCurveToPoint(path, NULL, 250, 100, 350, 200, 400, 300);

    // Add the bezier curve to the graphics context
    CGContextAddPath(context, path);

    // Set the line width for the bezier curve
    CGContextSetLineWidth(context, 5.0);

    // Stroke the bezier curve with the blue color
    CGContextStrokePath(context);

    // Create a gradient for the fill color
    CGGradientRef gradient = CGGradientCreateWithColors(CGColorSpaceCreateDeviceRGB(), (__bridge CFArrayRef)@[(id)[UIColor redColor].CGColor, (id)[UIColor blueColor].CGColor], NULL);

    // Set the gradient as the fill color
    CGContextSetFillColorWithGradient(context, gradient, CGPointMake(0, 0), CGPointMake(400, 400));

    // Fill a rectangle with the gradient
    CGContextFillRect(context, CGRectMake(0, 200, 400, 200));

    // Create a shadow for the text
    NSShadow *shadow = [[NSShadow alloc] init];
    shadow.shadowOffset = CGSizeMake(2.0, 2.0);
    shadow.shadowColor = [UIColor blackColor];

    // Set the shadow for the text
    CGContextSetShadowWithColor(context, shadow.shadowOffset, shadow.shadowBlurRadius, shadow.shadowColor.CGColor);

    // Set the text color to white
    CGContextSetTextColorWithColor(context, [UIColor whiteColor].CGColor);

    // Set the font for the text
    UIFont *font = [UIFont systemFontOfSize:24.0];
    CTFontRef fontRef = CTFontCreateWithName((__bridge CFStringRef)font.fontName, font.pointSize, NULL);

    // Create a Core Text frame for the text
    CTFrameRef frame = CTFramesetterCreateFrame(CTFramesetterCreateWithAttributedString((__bridge CFAttributedStringRef)@{
        (id)kCTFontAttributeName: (__bridge id)fontRef,
        (id)kCTForegroundColorAttributeName: (__bridge id)[UIColor whiteColor].CGColor
    }), CFRangeMake(0, 0), CGPathCreateWithRect(CGRectMake(0, 0, 400, 200)), NULL);

    // Draw the Core Text frame
    CTFrameDraw(frame, context);

    // Clean up
    CGGradientRelease(gradient);
    CGPathRelease(path);
    CTFontRelease(fontRef);
    CFRelease(frame);
}

@end

// Define a custom UIViewController subclass
@interface ViewController : UIViewController
@end

// Implement the ViewController class
@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];

    // Create an instance of the CustomView class
    CustomView *customView = [[CustomView alloc] initWithFrame:CGRectMake(0, 0, 320, 480)];

    // Add the custom view to the view controller's view
    [self.view addSubview:customView];
}

@end

// Define the main function
int main(int argc, char * argv[]) {
    // Create an instance of the UIApplication class
    UIApplication *application = [UIApplication sharedApplication];

    // Create an instance of the UIWindow class
    UIWindow *window = [[UIWindow alloc] initWithFrame:[UIScreen mainScreen].bounds];

    // Create an instance of the ViewController class
    ViewController *viewController = [[ViewController alloc] init];

    // Set the root view controller of the window to the view controller
    window.rootViewController = viewController;

    // Make the window visible
    [window makeKeyAndVisible];

    // Run the application
    return [application run];
}
```

This code creates a custom UIView subclass called CustomView. The CustomView class overrides the drawRect method to draw custom content. The drawRect method creates a graphics context, sets the fill and stroke colors, draws a rectangle with the red color, strokes a rectangle with the blue color, creates a path for a bezier curve, adds the bezier curve to the graphics context, sets the line width for the bezier curve, strokes the bezier curve with the blue color, creates a gradient for the fill color, sets the gradient as the fill color, fills a rectangle with the gradient, creates a shadow for the text, sets the shadow for the text, sets the text color to white, sets the font for the text, creates a Core Text frame for the text, draws the Core Text frame, and cleans up.

The ViewController class creates an instance of the CustomView class and adds it to the view controller's view.

The main function creates an instance of the UIApplication class, an instance of the UIWindow class, an instance of the ViewController class, sets the root view controller of the window to the view controller, makes the window visible, and runs the application.