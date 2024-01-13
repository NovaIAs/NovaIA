```rust
// This is a Rust program that simulates a simple traffic light system.
// It uses the `std::thread` module to create three threads, one for each light.
// The threads communicate with each other using channels, which are a type of
// message queue.

use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

// The `Light` enum represents the state of a traffic light.
enum Light {
    Red,
    Yellow,
    Green,
}

// The `TrafficLight` struct represents a traffic light system.
struct TrafficLight {
    // The `current_light` field stores the current state of the traffic light.
    current_light: Arc<Mutex<Light>>,

    // The `next_light` field stores the next state of the traffic light.
    next_light: Arc<Mutex<Light>>,

    // The `channel` field is a channel that is used to communicate with the
    // other threads.
    channel: std::sync::mpsc::Sender<Light>,
}

// The `run` method starts the traffic light system.
impl TrafficLight {
    fn run(&self) {
        // Start a thread that handles the current light.
        let current_light = self.current_light.clone();
        let channel = self.channel.clone();
        thread::spawn(move || {
            loop {
                // Get the current light state.
                let current_light = current_light.lock().unwrap();

                // Send the current light state to the channel.
                channel.send(*current_light).unwrap();

                // Wait for the next light state.
                let next_light = self.next_light.lock().unwrap();

                // Update the current light state.
                *current_light = *next_light;

                // Sleep for a while.
                thread::sleep(Duration::from_millis(1000));
            }
        });

        // Start a thread that handles the next light.
        let next_light = self.next_light.clone();
        let channel = self.channel.clone();
        thread::spawn(move || {
            loop {
                // Receive the next light state from the channel.
                let next_light = channel.recv().unwrap();

                // Update the next light state.
                *next_light = next_light;

                // Sleep for a while.
                thread::sleep(Duration::from_millis(1000));
            }
        });
    }
}

// The `main` function creates a traffic light system and starts it.
fn main() {
    // Create a new traffic light system.
    let traffic_light = TrafficLight {
        current_light: Arc::new(Mutex::new(Light::Red)),
        next_light: Arc::new(Mutex::new(Light::Green)),
        channel: std::sync::mpsc::channel(),
    };

    // Start the traffic light system.
    traffic_light.run();
}
```

This code is a Rust program that simulates a simple traffic light system. It uses the `std::thread` module to create three threads, one for each light. The threads communicate with each other using channels, which are a type of message queue.

The `Light` enum represents the state of a traffic light. It has three variants: `Red`, `Yellow`, and `Green`.

The `TrafficLight` struct represents a traffic light system. It has three fields:

* `current_light`: This field stores the current state of the traffic light.
* `next_light`: This field stores the next state of the traffic light.
* `channel`: This field is a channel that is used to communicate with the other threads.

The `run` method starts the traffic light system. It starts two threads, one for the current light and one for the next light. The current light thread sends the current light state to the channel every second. The next light thread receives the next light state from the channel every second and updates the next light state.

The `main` function creates a traffic light system and starts it.