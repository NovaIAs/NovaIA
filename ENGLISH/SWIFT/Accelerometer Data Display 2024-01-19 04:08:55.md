```swift
// Importing necessary frameworks
import UIKit
import CoreMotion

// Defining the main class of the application
class ViewController: UIViewController {

    // Initializing variables for accelerometer and motion manager
    var motionManager = CMMotionManager()
    var accelerometerData: CMAccelerometerData?

    // Initializing variables for user interface
    @IBOutlet weak var xAccelLabel: UILabel!
    @IBOutlet weak var yAccelLabel: UILabel!
    @IBOutlet weak var zAccelLabel: UILabel!

    // Function to handle accelerometer data updates
    func handleAccelerometerData(data: CMAccelerometerData) {
        // Updating accelerometer data
        accelerometerData = data

        // Updating UI labels with accelerometer data
        xAccelLabel.text = String(format: "%.2f", data.acceleration.x)
        yAccelLabel.text = String(format: "%.2f", data.acceleration.y)
        zAccelLabel.text = String(format: "%.2f", data.acceleration.z)
    }

    // Function to start accelerometer updates
    func startAccelerometerUpdates() {
        // Checking if accelerometer is available
        guard motionManager.isAccelerometerAvailable else {
            print("Accelerometer is not available on this device.")
            return
        }

        // Setting update interval for accelerometer
        motionManager.accelerometerUpdateInterval = 0.1

        // Starting accelerometer updates
        motionManager.startAccelerometerUpdates(to: OperationQueue.main, withHandler: handleAccelerometerData)
    }

    // Function to stop accelerometer updates
    func stopAccelerometerUpdates() {
        // Stopping accelerometer updates
        motionManager.stopAccelerometerUpdates()
    }

    // ViewDidLoad method
    override func viewDidLoad() {
        super.viewDidLoad()

        // Starting accelerometer updates
        startAccelerometerUpdates()
    }

    // ViewDidDisappear method
    override func viewDidDisappear(_ animated: Bool) {
        super.viewDidDisappear(animated)

        // Stopping accelerometer updates
        stopAccelerometerUpdates()
    }
}
```

This code creates a simple accelerometer application that displays real-time accelerometer data on the device's screen. It uses the CoreMotion framework to access the accelerometer data. Here's a brief explanation of the code:

1. Import necessary frameworks: The code starts by importing UIKit and CoreMotion frameworks, which are necessary for creating user interfaces and accessing motion data, respectively.

2. Define the main class (ViewController): The ViewController class is defined as the main class of the application. This is where the user interface and functionality of the app are implemented.

3. Initialize variables: The code initializes a few variables, including the motionManager, accelerometerData, and UI labels for displaying accelerometer data.

4. Handle accelerometer data updates: The handleAccelerometerData function is defined to handle accelerometer data updates. It receives accelerometer data as an argument and updates the accelerometerData variable and the UI labels with the latest data.

5. Start and stop accelerometer updates: The startAccelerometerUpdates and stopAccelerometerUpdates functions are defined to start and stop accelerometer updates, respectively. These functions use the motionManager to start and stop accelerometer data collection.

6. ViewDidLoad method: The viewDidLoad method is called when the view is loaded. It calls the startAccelerometerUpdates function to start collecting accelerometer data when the app is launched.

7. ViewDidDisappear method: The viewDidDisappear method is called when the view disappears. It calls the stopAccelerometerUpdates function to stop collecting accelerometer data when the app is closed or navigated away from.

This code allows you to create an app that displays real-time accelerometer data on the device's screen. You can extend this code further to add more features, such as saving the accelerometer data to a file or triggering specific actions based on the accelerometer data.