```
// Define a class representing a Vehicle
class Vehicle {
    private String type;
    private int numWheels;
    private double maxSpeed;

    // Constructor
    public Vehicle(String type, int numWheels, double maxSpeed) {
        this.type = type;
        this.numWheels = numWheels;
        this.maxSpeed = maxSpeed;
    }

    // Getter methods
    public String getType() {
        return type;
    }

    public int getNumWheels() {
        return numWheels;
    }

    public double getMaxSpeed() {
        return maxSpeed;
    }

    // Setter methods
    public void setType(String type) {
        this.type = type;
    }

    public void setNumWheels(int numWheels) {
        this.numWheels = numWheels;
    }

    public void setMaxSpeed(double maxSpeed) {
        this.maxSpeed = maxSpeed;
    }

    // Override the toString() method to provide a string representation of the object
    @Override
    public String toString() {
        return "Vehicle Type: " + type + ", Number of Wheels: " + numWheels + ", Maximum Speed: " + maxSpeed;
    }
}

// Define a class representing a Car, which inherits from the Vehicle class
class Car extends Vehicle {
    private int numSeats;

    // Constructor
    public Car(int numSeats, String type, int numWheels, double maxSpeed) {
        super(type, numWheels, maxSpeed);
        this.numSeats = numSeats;
    }

    // Getter/Setter methods for the numSeats field
    public int getNumSeats() {
        return numSeats;
    }

    public void setNumSeats(int numSeats) {
        this.numSeats = numSeats;
    }

    // Override the toString() method to provide a string representation of the object
    @Override
    public String toString() {
        return super.toString() + ", Number of Seats: " + numSeats;
    }
}

// Define a class representing a Truck, which inherits from the Vehicle class
class Truck extends Vehicle {
    private double cargoCapacity;

    // Constructor
    public Truck(double cargoCapacity, String type, int numWheels, double maxSpeed) {
        super(type, numWheels, maxSpeed);
        this.cargoCapacity = cargoCapacity;
    }

    // Getter/Setter methods for the cargoCapacity field
    public double getCargoCapacity() {
        return cargoCapacity;
    }

    public void setCargoCapacity(double cargoCapacity) {
        this.cargoCapacity = cargoCapacity;
    }

    // Override the toString() method to provide a string representation of the object
    @Override
    public String toString() {
        return super.toString() + ", Cargo Capacity: " + cargoCapacity;
    }
}

// Define a class representing a Garage, which can store a collection of Vehicles
class Garage {
    private List<Vehicle> vehicles;

    // Constructor
    public Garage() {
        this.vehicles = new ArrayList<Vehicle>();
    }

    // Add a vehicle to the garage
    public void addVehicle(Vehicle vehicle) {
        vehicles.add(vehicle);
    }

    // Remove a vehicle from the garage
    public void removeVehicle(Vehicle vehicle) {
        vehicles.remove(vehicle);
    }

    // Get a list of all vehicles in the garage
    public List<Vehicle> getVehicles() {
        return vehicles;
    }

    // Override the toString() method to provide a string representation of the object
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Garage Contents:\n");
        for (Vehicle vehicle : vehicles) {
            sb.append(vehicle.toString()).append("\n");
        }
        return sb.toString();
    }
}

// Main class to demonstrate the usage of the above classes
public class Main {
    public static void main(String[] args) {
        // Create a new Garage object
        Garage garage = new Garage();

        // Create a new Car object and add it to the garage
        Car car = new Car(5, "Sedan", 4, 120.0);
        garage.addVehicle(car);

        // Create a new Truck object and add it to the garage
        Truck truck = new Truck(1000.0, "Pickup", 6, 90.0);
        garage.addVehicle(truck);

        // Print out the contents of the garage
        System.out.println(garage);

        // Remove the truck from the garage
        garage.removeVehicle(truck);

        // Print out the contents of the garage again
        System.out.println(garage);
    }
}
```

Explanation:

* The `Vehicle` class serves as the base class for all types of vehicles. It defines common properties and behaviors of vehicles like type, number of wheels, and maximum speed.

* The `Car` and `Truck` classes extend the `Vehicle` class and represent specific types of vehicles. They have additional properties and behaviors specific to cars and trucks, respectively.

* The `Garage` class represents a garage that can store a collection of vehicles. It provides methods to add, remove, and retrieve vehicles from the garage.

* A `Garage` object is created, and a `Car` and a `Truck` object are added to the garage.

* Finally, the `Garage` object is printed using `toString()`, which prints out information about the garage's contents.