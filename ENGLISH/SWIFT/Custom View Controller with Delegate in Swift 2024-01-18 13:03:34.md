```swift
// Import necessary frameworks
import Foundation
import UIKit
import AVFoundation
import CoreLocation

// Define a protocol for a custom view controller
protocol CustomViewControllerDelegate: class {
    func customViewControllerDidFinish(_ controller: CustomViewController)
}

// Create a custom view controller
class CustomViewController: UIViewController {
    // MARK: - Properties
    weak var delegate: CustomViewControllerDelegate?
    
    // MARK: - Lifecycle Methods
    override func viewDidLoad() {
        super.viewDidLoad()
        
        // Setup the view
        view.backgroundColor = .white
        
        // Create a label
        let label = UILabel()
        label.text = "Hello, World!"
        label.textAlignment = .center
        label.font = UIFont.systemFont(ofSize: 20)
        label.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(label)
        
        // Create a button
        let button = UIButton()
        button.setTitle("Done", for: .normal)
        button.setTitleColor(.blue, for: .normal)
        button.addTarget(self, action: #selector(buttonTapped), for: .touchUpInside)
        button.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(button)
        
        // Create constraints
        NSLayoutConstraint.activate([
            label.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            label.centerYAnchor.constraint(equalTo: view.centerYAnchor),
            button.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            button.topAnchor.constraint(equalTo: label.bottomAnchor, constant: 20)
        ])
    }
    
    // MARK: - Actions
    @objc func buttonTapped() {
        // Notify the delegate that the view controller is finished
        delegate?.customViewControllerDidFinish(self)
    }
}

// Create a main view controller
class MainViewController: UIViewController {
    // MARK: - Properties
    private let customViewController = CustomViewController()
    
    // MARK: - Lifecycle Methods
    override func viewDidLoad() {
        super.viewDidLoad()
        
        // Setup the view
        view.backgroundColor = .white
        
        // Add the custom view controller as a child
        addChild(customViewController)
        customViewController.view.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(customViewController.view)
        customViewController.didMove(toParent: self)
        
        // Create constraints
        NSLayoutConstraint.activate([
            customViewController.view.topAnchor.constraint(equalTo: view.topAnchor),
            customViewController.view.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            customViewController.view.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            customViewController.view.bottomAnchor.constraint(equalTo: view.bottomAnchor)
        ])
        
        // Set the delegate for the custom view controller
        customViewController.delegate = self
    }
}

// MARK: - CustomViewControllerDelegate Methods
extension MainViewController: CustomViewControllerDelegate {
    func customViewControllerDidFinish(_ controller: CustomViewController) {
        // Dismiss the custom view controller
        controller.dismiss(animated: true, completion: nil)
    }
}

// Create an instance of the main view controller and present it
let mainViewController = MainViewController()
mainViewController.modalPresentationStyle = .fullScreen
UIApplication.shared.windows.first?.rootViewController?.present(mainViewController, animated: true, completion: nil)
```

**Explanation:**

This code creates a custom view controller with a label and a button. When the button is tapped, the delegate of the view controller is notified that the view controller is finished.

The main view controller adds the custom view controller as a child and sets itself as the delegate of the custom view controller.

When the button in the custom view controller is tapped, the main view controller is notified and dismisses the custom view controller.

Overall, this code demonstrates how to create a custom view controller, add it as a child to another view controller, and communicate between the view controllers using a delegate.