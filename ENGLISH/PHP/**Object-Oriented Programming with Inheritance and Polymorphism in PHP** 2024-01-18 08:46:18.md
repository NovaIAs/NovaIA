```php
// Define a class to represent a vehicle
class Vehicle
{
    protected $make;
    protected $model;
    protected $year;

    public function __construct($make, $model, $year)
    {
        $this->make = $make;
        $this->model = $model;
        $this->year = $year;
    }

    public function getMake()
    {
        return $this->make;
    }

    public function getModel()
    {
        return $this->model;
    }

    public function getYear()
    {
        return $this->year;
    }

    public function __toString()
    {
        return $this->make . " " . $this->model . " (" . $this->year . ")";
    }
}

// Define a class to represent a car
class Car extends Vehicle
{
    protected $numDoors;

    public function __construct($make, $model, $year, $numDoors)
    {
        parent::__construct($make, $model, $year);
        $this->numDoors = $numDoors;
    }

    public function getNumDoors()
    {
        return $this->numDoors;
    }

    public function __toString()
    {
        return parent::__toString() . ", " . $this->numDoors . " doors";
    }
}

// Define a class to represent a truck
class Truck extends Vehicle
{
    protected $payloadCapacity;

    public function __construct($make, $model, $year, $payloadCapacity)
    {
        parent::__construct($make, $model, $year);
        $this->payloadCapacity = $payloadCapacity;
    }

    public function getPayloadCapacity()
    {
        return $this->payloadCapacity;
    }

    public function __toString()
    {
        return parent::__toString() . ", " . $this->payloadCapacity . " pounds payload capacity";
    }
}

// Create a few vehicles
$car1 = new Car("Honda", "Civic", 2019, 4);
$car2 = new Car("Toyota", "Camry", 2018, 4);
$truck1 = new Truck("Ford", "F-150", 2017, 10000);
$truck2 = new Truck("Chevrolet", "Silverado", 2016, 12000);

// Display the vehicles
echo "Car 1: " . $car1 . PHP_EOL;
echo "Car 2: " . $car2 . PHP_EOL;
echo "Truck 1: " . $truck1 . PHP_EOL;
echo "Truck 2: " . $truck2 . PHP_EOL;
```

Explanation:

* The `Vehicle` class is a parent class that defines common properties and methods for all vehicles.
* The `Car` and `Truck` classes are child classes that inherit from the `Vehicle` class and define additional properties and methods specific to cars and trucks.
* The `Car` class has a `numDoors` property and a `getNumDoors()` method to get the number of doors on the car.
* The `Truck` class has a `payloadCapacity` property and a `getPayloadCapacity()` method to get the payload capacity of the truck.
* The `__toString()` method is defined in the `Vehicle` class and is overridden in the `Car` and `Truck` classes. This method returns a string representation of the vehicle.
* The `create()` method is used to create new vehicles.
* The `display()` method is used to display the vehicles.

This code demonstrates the principles of inheritance and polymorphism in PHP.