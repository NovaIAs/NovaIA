```swift
// Import necessary frameworks
import Foundation
import UIKit
import AVFoundation

// Define a custom UIView subclass for the video player
class VideoPlayerView: UIView {
    
    // Initialize the video player with a video URL
    init(videoURL: URL) {
        super.init(frame: .zero)
        
        // Create an AVPlayer instance to play the video
        let player = AVPlayer(url: videoURL)
        
        // Create an AVPlayerLayer to display the video
        let playerLayer = AVPlayerLayer(player: player)
        playerLayer.frame = self.bounds
        
        // Add the player layer to the view
        self.layer.addSublayer(playerLayer)
        
        // Start playing the video
        player.play()
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

// Define a custom UIViewController subclass for the video player controller
class VideoPlayerViewController: UIViewController {
    
    // Initialize the video player controller with a video URL
    init(videoURL: URL) {
        super.init(nibName: nil, bundle: nil)
        
        // Create a video player view and add it to the view controller's view
        let videoPlayerView = VideoPlayerView(videoURL: videoURL)
        videoPlayerView.frame = self.view.bounds
        self.view.addSubview(videoPlayerView)
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}

// Usage:
// Create a video player view controller with a video URL
let videoURL = URL(string: "https://www.example.com/video.mp4")!
let videoPlayerViewController = VideoPlayerViewController(videoURL: videoURL)

// Present the video player view controller modally
self.present(videoPlayerViewController, animated: true, completion: nil)
```

**Explanation:**

This code creates a custom video player using AVFoundation in Swift. It defines a `VideoPlayerView` class that inherits from `UIView` and initializes an `AVPlayer` and `AVPlayerLayer` to play and display the video. The `VideoPlayerViewController` class inherits from `UIViewController` and creates a `VideoPlayerView` instance to display the video. It also provides an example of how to use the video player controller by creating an instance and presenting it modally.