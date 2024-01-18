// Import necessary UIKit and AVFoundation libraries
import UIKit
import AVFoundation

// Define a class that extends UIViewController and conforms to AVAudioPlayerDelegate
class CustomAudioPlayerViewController: UIViewController, AVAudioPlayerDelegate {

    // Variable to store the audio player object
    var audioPlayer: AVAudioPlayer?

    // Initialize the audio session
    override func viewDidLoad() {
        super.viewDidLoad()

        // Create a URL to the audio file
        let audioURL = Bundle.main.url(forResource: "audio", withExtension: "mp3")!

        // Initialize the audio player with the URL
        audioPlayer = try? AVAudioPlayer(contentsOf: audioURL)
        
        // Set the audio player's delegate to self
        audioPlayer?.delegate = self
    }

    // MARK: - IBActions

    // Function to handle the play button being pressed
    @IBAction func playButtonTapped(_ sender: UIButton) {
        // Check if the audio player is playing, and if not, start playing
        if !(audioPlayer?.isPlaying ?? false) {
            audioPlayer?.play()
        }
    }

    // Function to handle the pause button being pressed
    @IBAction func pauseButtonTapped(_ sender: UIButton) {
        // Check if the audio player is playing, and if so, pause it
        if (audioPlayer?.isPlaying ?? false) {
            audioPlayer?.pause()
        }
    }

    // MARK: - AVAudioPlayerDelegate Methods

    // Function that is called when the audio player has finished playing
    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        // Check if the playback was successful
        if flag {
            // Playback was successful, reset the audio player's position to the beginning
            audioPlayer?.currentTime = 0
        }
    }
}