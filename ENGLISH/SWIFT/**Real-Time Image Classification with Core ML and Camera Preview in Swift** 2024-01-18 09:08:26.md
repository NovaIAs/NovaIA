```swift
// Import necessary frameworks
import UIKit
import AVFoundation
import CoreML

// Main class
class ViewController: UIViewController {

    // UI elements
    @IBOutlet weak var cameraView: UIView!
    @IBOutlet weak var predictButton: UIButton!
    @IBOutlet weak var resultLabel: UILabel!

    // Model and AVFoundation objects
    var model: VNCoreMLModel!
    var videoCapture: AVCaptureSession!
    var cameraLayer: AVCaptureVideoPreviewLayer!

    override func viewDidLoad() {
        super.viewDidLoad()
        initializeModel()
        setupCamera()
    }

    private func initializeModel() {
        // Load the Core ML model
        let modelURL = Bundle.main.url(forResource: "ResNet50", withExtension: "mlmodelc")!
        model = try! VNCoreMLModel(for: MLModel(contentsOf: modelURL))
    }

    private func setupCamera() {
        // Create the video capture session
        videoCapture = AVCaptureSession()
        videoCapture.sessionPreset = .hd1280x720

        // Add the camera input
        let cameraDevice = AVCaptureDevice.default(.builtInWideAngleCamera, for: .video, position: .back)!
        let cameraInput = try! AVCaptureDeviceInput(device: cameraDevice)
        videoCapture.addInput(cameraInput)

        // Add the video capture output
        let videoOutput = AVCaptureVideoDataOutput()
        videoOutput.videoSettings = [kCVPixelBufferPixelFormatTypeKey as String: kCVPixelFormatType_32BGRA]
        videoOutput.setSampleBufferDelegate(self, queue: DispatchQueue.global(qos: .userInteractive))
        videoCapture.addOutput(videoOutput)

        // Add camera preview layer
        cameraLayer = AVCaptureVideoPreviewLayer(session: videoCapture)
        cameraLayer.videoGravity = .resizeAspectFill
        cameraLayer.frame = cameraView.bounds
        cameraView.layer.addSublayer(cameraLayer)

        // Start the camera capture session
        videoCapture.startRunning()
    }

    @IBAction func predictButtonPressed(_ sender: UIButton) {
        // Take a snapshot from camera
        let snapshot = cameraLayer.connection?.snapshot()!
        
        // Convert snapshot to CVPixelBuffer
        let pixelBuffer = snapshot.pixelBuffer!

        // Create Core ML request
        let request = VNCoreMLRequest(model: model) { request, error in
            guard let results = request.results as? [VNClassificationObservation],
                let topResult = results.first else {
                self.resultLabel.text = "No results"
                return
            }
            
            // Display prediction
            self.resultLabel.text = "\(topResult.identifier) - \(String(format: "%.2f", topResult.confidence))%"
        }
        
        // Perform request
        let handler = VNImageRequestHandler(cvPixelBuffer: pixelBuffer, options: [:])
        try! handler.perform([request])
    }
}

// Handle video capture delegate methods
extension ViewController: AVCaptureVideoDataOutputSampleBufferDelegate {
    func captureOutput(_ output: AVCaptureOutput, didOutput sampleBuffer: CMSampleBuffer, from connection: AVCaptureConnection) {
        // Get the pixel buffer from the sample buffer
        let pixelBuffer = CMSampleBufferGetImageBuffer(sampleBuffer)!

        // Create Core ML request
        let request = VNCoreMLRequest(model: model) { request, error in
            guard let results = request.results as? [VNClassificationObservation],
                let topResult = results.first else {
                return
            }
            
            // Update UI with prediction on main thread
            DispatchQueue.main.async {
                self.resultLabel.text = "\(topResult.identifier) - \(String(format: "%.2f", topResult.confidence))%"
            }
        }

        // Perform request
        let handler = VNImageRequestHandler(cvPixelBuffer: pixelBuffer, options: [:])
        try! handler.perform([request])
    }
}
```

This code is a complex and differentiated Swift program that performs real-time image classification using a Core ML model. It features a camera preview, a prediction button, and a label to display the classification results. The code is explained below:

1. **Model Initialization:** In the `initializeModel()` method, the program loads a Core ML model named "ResNet50" from a bundle. This model is used for image classification.

2. **Camera Setup:** The `setupCamera()` method initializes the `AVCaptureSession`, adds the camera input and output, and creates the `AVCaptureVideoPreviewLayer` to display the camera preview.

3. **Prediction Button Action:** When the "Predict" button is pressed, the program takes a snapshot from the camera and converts it into a `CVPixelBuffer`. It then creates a `VNCoreMLRequest` with the `model` to perform the classification.

4. **Video Capture Delegate Methods:** The `AVCaptureVideoDataOutputSampleBufferDelegate` protocol is implemented to handle video capture delegate methods. It captures frames from the camera, converts them to `CVPixelBuffer`, and performs classification using the `VNCoreMLRequest`.

5. **User Interface:** The program displays the classification result in the `resultLabel`.

This program demonstrates the use of Core ML for real-time image classification, making it a complex and differentiated code example.