```swift
// Import the necessary modules.
import Foundation
import UIKit

// Define a custom UIView subclass.
class MyCustomView: UIView {
    
    // Define the view's properties.
    var title: String
    var subtitle: String
    var image: UIImage?
    
    // Initialize the view with the specified properties.
    init(title: String, subtitle: String, image: UIImage? = nil) {
        self.title = title
        self.subtitle = subtitle
        self.image = image
        
        super.init(frame: .zero)
        
        // Setup the view's layout.
        setupView()
    }
    
    // Required initializer for Interface Builder.
    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    // Setup the view's layout.
    private func setupView() {
        // Create a stack view to hold the title and subtitle labels.
        let stackView = UIStackView()
        stackView.axis = .vertical
        stackView.spacing = 8
        
        // Create a label for the title.
        let titleLabel = UILabel()
        titleLabel.text = title
        titleLabel.font = UIFont.boldSystemFont(ofSize: 17)
        
        // Create a label for the subtitle.
        let subtitleLabel = UILabel()
        subtitleLabel.text = subtitle
        subtitleLabel.font = UIFont.systemFont(ofSize: 15)
        
        // Add the labels to the stack view.
        stackView.addArrangedSubview(titleLabel)
        stackView.addArrangedSubview(subtitleLabel)
        
        // Add the stack view to the view.
        addSubview(stackView)
        
        // Add the image view to the view.
        if let image = image {
            let imageView = UIImageView(image: image)
            imageView.contentMode = .scaleAspectFit
            addSubview(imageView)
        }
        
        // Add constraints to the subviews.
        stackView.translatesAutoresizingMaskIntoConstraints = false
        stackView.centerXAnchor.constraint(equalTo: centerXAnchor).isActive = true
        stackView.centerYAnchor.constraint(equalTo: centerYAnchor).isActive = true
        stackView.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 16).isActive = true
        stackView.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -16).isActive = true
        
        if let imageView = imageView {
            imageView.translatesAutoresizingMaskIntoConstraints = false
            imageView.topAnchor.constraint(equalTo: topAnchor, constant: 16).isActive = true
            imageView.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 16).isActive = true
            imageView.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -16).isActive = true
            imageView.heightAnchor.constraint(equalToConstant: 100).isActive = true
        }
    }
}

// Create an instance of the custom view.
let myView = MyCustomView(title: "My Custom View", subtitle: "This is a custom view.", image: UIImage(named: "image.png"))

// Add the view to a view controller's view.
let viewController = UIViewController()
viewController.view.addSubview(myView)

// Set the view's frame.
myView.frame = CGRect(x: 0, y: 0, width: 300, height: 200)

// Congratulations, you've created a custom view in Swift!
```

**Explanation:**

This code creates a custom UIView subclass called `MyCustomView`. This subclass has three properties: `title`, `subtitle`, and `image`. The `init` method initializes the view with the specified properties and sets up the view's layout.

The `setupView` method creates a stack view to hold the title and subtitle labels, adds the labels to the stack view, and adds the stack view to the view. It also adds the image view to the view if an image is specified.

The `MyCustomView` class has a required initializer for Interface Builder, which is a tool that allows you to design user interfaces in Xcode.

The `viewDidLoad` method of the view controller creates an instance of the `MyCustomView` class, adds it to the view controller's view, and sets the view's frame.

This code demonstrates how to create a custom UIView subclass in Swift and use it in a view controller.