```swift
// Import necessary frameworks
import Foundation
import UIKit

// Define a custom UIView subclass
class MyCustomView: UIView {
    
    // Override the drawRect method to draw custom content
    override func drawRect(rect: CGRect) {
        
        // Get the current graphics context
        let context = UIGraphicsGetCurrentContext()
        
        // Set the fill color to red
        CGContextSetFillColorWithColor(context, UIColor.redColor().CGColor)
        
        // Fill a rectangle with the current fill color
        CGContextFillRect(context, CGRectMake(0, 0, 100, 100))
        
        // Set the stroke color to blue
        CGContextSetStrokeColorWithColor(context, UIColor.blueColor().CGColor)
        
        // Set the line width to 5 points
        CGContextSetLineWidth(context, 5.0)
        
        // Draw a rectangle with the current stroke color and line width
        CGContextStrokeRectWithWidth(context, CGRectMake(100, 100, 100, 100), 5.0)
        
        // Set the shadow color to black
        CGContextSetShadowWithColor(context, CGSizeMake(10, 10), 5, UIColor.blackColor().CGColor)
        
        // Draw a text string with the current shadow settings
        let text = "Hello, world!"
        let attributes = [
            NSFontAttributeName: UIFont.systemFontOfSize(24),
            NSForegroundColorAttributeName: UIColor.whiteColor()
        ]
        let size = text.sizeWithAttributes(attributes)
        let point = CGPointMake((rect.width - size.width) / 2, (rect.height - size.height) / 2)
        text.drawAtPoint(point, withAttributes: attributes)
    }
}

// Create an instance of the custom view
let customView = MyCustomView(frame: CGRectMake(0, 0, 200, 200))

// Add the custom view to the view controller's view
self.view.addSubview(customView)
```

This code creates a custom UIView subclass called `MyCustomView` that draws a red rectangle, a blue rectangle with a 5-point border, and a black-shadowed text string. The code then creates an instance of the custom view and adds it to the view controller's view.

Here is a breakdown of the code:

* The `import` statements at the beginning of the code import the necessary frameworks.
* The `MyCustomView` class is defined as a subclass of `UIView`.
* The `drawRect` method is overridden to draw custom content in the view.
* The `UIGraphicsGetCurrentContext()` function is called to get the current graphics context.
* The `CGContextSetFillColorWithColor` function is called to set the fill color to red.
* The `CGContextFillRect` function is called to fill a rectangle with the current fill color.
* The `CGContextSetStrokeColorWithColor` function is called to set the stroke color to blue.
* The `CGContextSetLineWidth` function is called to set the line width to 5 points.
* The `CGContextStrokeRectWithWidth` function is called to draw a rectangle with the current stroke color and line width.
* The `CGContextSetShadowWithColor` function is called to set the shadow color to black.
* The `text` variable is assigned a string of text.
* The `attributes` dictionary is created to specify the font and color of the text.
* The `size` variable is assigned the size of the text string with the specified attributes.
* The `point` variable is assigned the center point of the view.
* The `text` string is drawn at the specified point with the specified attributes.
* An instance of the `MyCustomView` class is created and added to the view controller's view.