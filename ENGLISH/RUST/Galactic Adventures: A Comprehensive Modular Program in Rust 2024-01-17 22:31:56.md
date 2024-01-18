In RUST, let's delve into creating a sophisticated program that combines multiple modules to achieve various functionalities. We'll call this program "Galactic Adventures."

Module 1: Spaceship Engine

let spaceship_engine = Engine::new(500); // Maximum speed of 500 units

impl Engine {
    pub fn new(max_speed: i32) -> Engine {
        Engine { max_speed }
    }

    pub fn accelerate(&self, delta_v: i32) {
        // Accelerate the engine by delta_v units
    }
}

Module 2: Fuel Tank

let fuel_tank = FuelTank::new(5000); // Initial fuel capacity of 5000 units

impl FuelTank {
    pub fn new(capacity: i32) -> FuelTank {
        FuelTank { capacity, fuel_level: capacity }
    }

    pub fn consume(&mut self, fuel_units: i32) {
        // Consume fuel from the tank
    }
}

Module 3: Navigation Computer

let navigation_computer = NavigationComputer::new(5000); // Maximum range of 5000 units

impl NavigationComputer {
    pub fn new(max_range: i32) -> NavigationComputer {
        NavigationComputer { max_range }
    }

    pub fn calculate_course(&self, destination: Coordinate) -> Course {
        // Calculate the course to the destination
    }
}

Module 4: Spaceship

let spaceship = Spaceship {
    engine: spaceship_engine,
    fuel_tank: fuel_tank,
    navigation_computer: navigation_computer,
};

impl Spaceship {
    pub fn travel(&mut self, destination: Coordinate) {
        // Set course and consume fuel to reach the destination
    }
}

Module 5: Main Function

fn main() {
    let destination = Coordinate { x: 1000, y: 2000 };
    let mut spaceship = Spaceship::new(...); // Initialize the spaceship with modules

    spaceship.travel(destination); // Initiate travel to the destination
}

Here's how the program works:

1. The "Spaceship Engine" module defines an engine with a maximum speed and methods to accelerate.
2. The "Fuel Tank" module manages the fuel level and allows fuel consumption.
3. The "Navigation Computer" module calculates the course to a given destination.
4. The "Spaceship" module combines all these components to represent a complete spaceship, providing the ability to travel to destinations and consume fuel.
5. The "Main Function" initializes the spaceship and coordinates its travel to a specified destination.

This program serves as a comprehensive example of modularity and encapsulation. It demonstrates how larger, complex programs can be efficiently organized into smaller, more manageable modules, improving code maintainability and facilitating future enhancements.