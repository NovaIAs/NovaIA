```swift
// Import the necessary frameworks
import Foundation
import AVFoundation
import CoreMotion
import CoreLocation

// Define a class to handle the audio recording and playback
class AudioRecorder {

    // Declare the properties of the class
    private let audioSession: AVAudioSession
    private let audioRecorder: AVAudioRecorder
    private let audioPlayer: AVAudioPlayer

    // Initialize the class
    init() throws {
        // Set up the audio session
        try audioSession.setCategory(.playAndRecord, mode: .default)
        try audioSession.setActive(true)

        // Create the audio recorder
        let settings = [
            AVFormatIDKey: kAudioFormatMPEG4AAC,
            AVSampleRateKey: 44100,
            AVNumberOfChannelsKey: 2
        ]
        audioRecorder = try AVAudioRecorder(url: URL(fileURLWithPath: "recording.m4a"), settings: settings)

        // Create the audio player
        audioPlayer = try AVAudioPlayer(contentsOf: URL(fileURLWithPath: "recording.m4a"))
    }

    // Start recording audio
    func startRecording() {
        audioRecorder.record()
    }

    // Stop recording audio
    func stopRecording() {
        audioRecorder.stop()
    }

    // Play the recorded audio
    func playRecording() {
        audioPlayer.play()
    }

    // Stop playing the recorded audio
    func stopPlaying() {
        audioPlayer.stop()
    }
}

// Define a class to handle the motion detection
class MotionDetector {

    // Declare the properties of the class
    private let motionManager: CMMotionManager

    // Initialize the class
    init() {
        // Create the motion manager
        motionManager = CMMotionManager()

        // Start the motion updates
        motionManager.startAccelerometerUpdates(to: OperationQueue.main) { (data, error) in
            // Handle the accelerometer data
            if let data = data {
                // Print the acceleration values
                print("Acceleration: \(data.acceleration)")
            }
        }
    }

    // Stop the motion updates
    func stopMotionUpdates() {
        motionManager.stopAccelerometerUpdates()
    }
}

// Define a class to handle the location tracking
class LocationTracker {

    // Declare the properties of the class
    private let locationManager: CLLocationManager

    // Initialize the class
    init() {
        // Create the location manager
        locationManager = CLLocationManager()

        // Request permission to access the user's location
        locationManager.requestWhenInUseAuthorization()

        // Start the location updates
        locationManager.startUpdatingLocation()
    }

    // Stop the location updates
    func stopLocationUpdates() {
        locationManager.stopUpdatingLocation()
    }
}

// Create an instance of the audio recorder
let audioRecorder = AudioRecorder()

// Start recording audio
audioRecorder.startRecording()

// Create an instance of the motion detector
let motionDetector = MotionDetector()

// Create an instance of the location tracker
let locationTracker = LocationTracker()

// Wait for 10 seconds
sleep(10)

// Stop recording audio
audioRecorder.stopRecording()

// Stop the motion updates
motionDetector.stopMotionUpdates()

// Stop the location updates
locationTracker.stopLocationUpdates()

// Play the recorded audio
audioRecorder.playRecording()

// Wait for the audio to finish playing
while audioRecorder.isPlaying {}

// Print the recorded audio file path
print("Recording file path: \(audioRecorder.audioRecorder.url)")
```

This code performs the following tasks:

1. It sets up an audio session, creates an audio recorder, and an audio player.
2. It starts recording audio.
3. It starts the motion updates.
4. It starts the location updates.
5. It waits for 10 seconds.
6. It stops recording audio, the motion updates, and the location updates.
7. It plays the recorded audio.
8. It prints the recorded audio file path.

This code is complex because it involves multiple frameworks, including AVFoundation, CoreMotion, and CoreLocation. It also uses multiple threads to handle the audio recording, motion detection, and location tracking simultaneously.