// Import necessary libraries
import UIKit
import CoreMotion

// Create a class to handle motion events
class MotionManager: UIViewController {
    
    // Create a motion manager object
    private let motionManager = CMMotionManager()
    
    // Create a variable to store the current orientation
    private var currentOrientation: UIDeviceOrientation?

    override func viewDidLoad() {
        super.viewDidLoad()

        // Start accelerometer updates
        motionManager.startAccelerometerUpdates()
        
        // Start gyroscope updates
        motionManager.startGyroUpdates()
        
        // Start magnetometer updates
        motionManager.startMagnetometerUpdates()
        
        // Start device motion updates
        motionManager.startDeviceMotionUpdates()
        
        // Set the update interval
        motionManager.accelerometerUpdateInterval = 0.1
        motionManager.gyroUpdateInterval = 0.1
        motionManager.magnetometerUpdateInterval = 0.1
        motionManager.deviceMotionUpdateInterval = 0.1
        
        // Listen for device orientation changes
        NotificationCenter.default.addObserver(self, selector: #selector(deviceOrientationDidChange), name: UIDevice.orientationDidChangeNotification, object: nil)
    }
    
    // Function to handle device orientation changes
    @objc func deviceOrientationDidChange() {
        currentOrientation = UIDevice.current.orientation
    }
    
    // Function to get the current acceleration data
    func getAccelerationData() -> CMAcceleration {
        return motionManager.accelerometerData!.acceleration
    }
    
    // Function to get the current gyroscope data
    func getGyroscopeData() -> CMRotationRate {
        return motionManager.gyroData!.rotationRate
    }
    
    // Function to get the current magnetometer data
    func getMagnetometerData() -> CMMagneticField {
        return motionManager.magnetometerData!.magneticField
    }
    
    // Function to get the current device motion data
    func getDeviceMotionData() -> CMDeviceMotion {
        return motionManager.deviceMotion!
    }
    
    // Function to get the current orientation
    func getCurrentOrientation() -> UIDeviceOrientation {
        if let orientation = currentOrientation {
            return orientation
        } else {
            return UIDevice.current.orientation
        }
    }
    
    // Function to stop all motion updates
    func stopAllMotionUpdates() {
        motionManager.stopAccelerometerUpdates()
        motionManager.stopGyroUpdates()
        motionManager.stopMagnetometerUpdates()
        motionManager.stopDeviceMotionUpdates()
    }
}

// Create an instance of the motion manager
let motionManager = MotionManager()

// Get the current acceleration data
let accelerationData = motionManager.getAccelerationData()

// Get the current gyroscope data
let gyroscopeData = motionManager.getGyroscopeData()

// Get the current magnetometer data
let magnetometerData = motionManager.getMagnetometerData()

// Get the current device motion data
let deviceMotionData = motionManager.getDeviceMotionData()

// Get the current orientation
let orientation = motionManager.getCurrentOrientation()

// Print the data to the console
print("Acceleration: \(accelerationData)")
print("Gyroscope: \(gyroscopeData)")
print("Magnetometer: \(magnetometerData)")
print("Device Motion: \(deviceMotionData)")
print("Orientation: \(orientation)")

// Stop all motion updates
motionManager.stopAllMotionUpdates()

// This code uses the Core Motion framework to get data from the accelerometer, gyroscope, magnetometer, and device motion sensors. It then prints the data to the console. The code can be used to create apps that track the user's motion.