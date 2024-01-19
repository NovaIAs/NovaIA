import UIKit
import SceneKit

// Create a new SceneKit view
let scnView = SCNView()
scnView.frame = CGRect(x: 0, y: 0, width: 1024, height: 768)

// Create a new SceneKit scene
let scene = SCNScene()

// Create a new camera node
let cameraNode = SCNNode()
cameraNode.camera = SCNCamera()
cameraNode.position = SCNVector3(x: 0, y: 0, z: 10)

// Add the camera node to the scene
scene.rootNode.addChildNode(cameraNode)

// Create a new box node
let boxNode = SCNNode(geometry: SCNBox(width: 1, height: 1, length: 1))
boxNode.position = SCNVector3(x: 0, y: 0, z: 0)

// Add the box node to the scene
scene.rootNode.addChildNode(boxNode)

// Create a new light node
let lightNode = SCNNode()
lightNode.light = SCNLight()
lightNode.light!.type = SCNLight.LightType.omni
lightNode.position = SCNVector3(x: 0, y: 10, z: 10)

// Add the light node to the scene
scene.rootNode.addChildNode(lightNode)

// Set the scene to the SceneKit view
scnView.scene = scene

// Add the SceneKit view to the view controller's view
self.view.addSubview(scnView)

// Animate the box node
let animation = CABasicAnimation(keyPath: "position")
animation.fromValue = NSValue(scnVector3: SCNVector3(x: 0, y: 0, z: 0))
animation.toValue = NSValue(scnVector3: SCNVector3(x: 10, y: 10, z: 10))
animation.duration = 5
animation.repeatCount = Float.infinity

boxNode.addAnimation(animation, forKey: "position")

// Allow the user to interact with the scene
scnView.allowsCameraControl = true

// Start the SceneKit view's rendering loop
scnView.start()

// Create a new button
let button = UIButton(type: .system)
button.frame = CGRect(x: 100, y: 100, width: 100, height: 50)
button.setTitle("Click Me", for: .normal)
button.addTarget(self, action: #selector(buttonTapped), for: .touchUpInside)

// Add the button to the view controller's view
self.view.addSubview(button)

// Define the buttonTapped method
@objc func buttonTapped() {
    // Stop the SceneKit view's rendering loop
    scnView.stop()

    // Create a new UIAlertController
    let alert = UIAlertController(title: "Button Tapped", message: "You tapped the button!", preferredStyle: .alert)

    // Create a new UIAlertAction
    let okAction = UIAlertAction(title: "OK", style: .default) { (action) in
        // Start the SceneKit view's rendering loop again
        scnView.start()
    }

    // Add the UIAlertAction to the UIAlertController
    alert.addAction(okAction)

    // Present the UIAlertController
    self.present(alert, animated: true)
}

// This code creates a simple SceneKit scene with a box, a camera, and a light. The box is animated to move around the scene. The user can interact with the scene by clicking and dragging the mouse. When the user clicks the button, the scene is paused and a UIAlertController is presented. When the user clicks the OK button on the UIAlertController, the scene is resumed.

Here is a brief explanation of the code:

* The import statements import the necessary libraries for SceneKit and UIKit.
* The scnView variable is created to hold the SceneKit view.
* The scene variable is created to hold the SceneKit scene.
* The cameraNode variable is created to hold the camera node.
* The boxNode variable is created to hold the box node.
* The lightNode variable is created to hold the light node.
* The SceneKit view is added to the view controller's view.
* The box node is animated to move around the scene.
* The user is allowed to interact with the scene.
* The start method is called on the SceneKit view to start the rendering loop.
* A button is created and added to the view controller's view.
* The buttonTapped method is defined to handle the button tap event.
* When the button is tapped, the SceneKit view is stopped, a UIAlertController is presented, and the scene is resumed when the user clicks the OK button on the UIAlertController.