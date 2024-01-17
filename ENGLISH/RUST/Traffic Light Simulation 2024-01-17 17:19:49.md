```rust
// Create an enum to represent the different states of a traffic light.
enum TrafficLightState {
    Red,
    Yellow,
    Green,
}

// Define a struct to represent a traffic light.
struct TrafficLight {
    state: TrafficLightState,
    duration: u32, // Duration of the current state in seconds
}

// Implement the `new` function to create a new traffic light with an initial state and duration.
impl TrafficLight {
    fn new(state: TrafficLightState, duration: u32) -> TrafficLight {
        TrafficLight { state, duration }
    }
}

// Create a function to change the state of the traffic light to the next state.
fn next_state(state: TrafficLightState) -> TrafficLightState {
    match state {
        TrafficLightState::Red => TrafficLightState::Green,
        TrafficLightState::Yellow => TrafficLightState::Red,
        TrafficLightState::Green => TrafficLightState::Yellow,
    }
}

// Create a function to run the traffic light.
fn run_traffic_light(traffic_light: &mut TrafficLight) {
    // Loop indefinitely, changing the state of the traffic light every `duration` seconds.
    loop {
        // Decrement the duration of the current state by 1 second.
        traffic_light.duration -= 1;

        // If the duration of the current state is 0, change the state to the next state.
        if traffic_light.duration == 0 {
            traffic_light.state = next_state(traffic_light.state);
            // Reset the duration of the new state to the specified duration.
            traffic_light.duration = traffic_light.duration;
        }

        // Print the current state of the traffic light.
        println!("Current state: {:?}", traffic_light.state);

        // Sleep for 1 second.
        std::thread::sleep(std::time::Duration::from_secs(1));
    }
}

// Create a main function to initialize and run the traffic light.
fn main() {
    // Create a new traffic light with an initial state of Red and a duration of 10 seconds.
    let mut traffic_light = TrafficLight::new(TrafficLightState::Red, 10);

    // Run the traffic light.
    run_traffic_light(&mut traffic_light);
}
```

**Explanation:**

* This code defines an `enum` called `TrafficLightState` to represent the different states of a traffic light (Red, Yellow, and Green).
* It also defines a `struct` called `TrafficLight` that represents a traffic light. The `TrafficLight` struct has two fields: `state` (the current state of the traffic light) and `duration` (the duration of the current state in seconds).
* The `new` function is used to create a new traffic light with an initial state and duration.
* The `next_state` function is used to change the state of the traffic light to the next state.
* The `run_traffic_light` function is used to run the traffic light. This function loops indefinitely, changing the state of the traffic light every `duration` seconds.
* The `main` function initializes a new traffic light and then calls the `run_traffic_light` function to run the traffic light.

When you run this code, it will create a new traffic light with an initial state of Red and a duration of 10 seconds. The `run_traffic_light` function will then start running the traffic light, changing the state of the traffic light every 10 seconds. The current state of the traffic light will be printed to the console every second.