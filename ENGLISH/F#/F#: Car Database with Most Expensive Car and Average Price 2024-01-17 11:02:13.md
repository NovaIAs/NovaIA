```f#
// Define a type to represent a car.
type Car = {
    make: string;
    model: string;
    year: int;
    color: string;
    price: decimal;
}

// Define a function to create a new car.
let createCar make model year color price =
    {
        make = make;
        model = model;
        year = year;
        color = color;
        price = price;
    }

// Define a list of cars.
let cars = [
    createCar "Toyota" "Camry" 2020 "White" 25000m;
    createCar "Honda" "Accord" 2021 "Black" 28000m;
    createCar "Ford" "Mustang" 2022 "Red" 35000m;
    createCar "Chevrolet" "Corvette" 2023 "Blue" 60000m;
]

// Define a function to print the details of a car.
let printCar car =
    printfn "%s %s %i %s %f" car.make car.model car.year car.color car.price

// Print the details of each car in the list.
List.iter printCar cars

// Define a function to find the most expensive car.
let findMostExpensiveCar cars =
    cars |> List.maxBy (fun car -> car.price)

// Find the most expensive car and print its details.
let mostExpensiveCar = findMostExpensiveCar cars
printfn "Most Expensive Car:"
printCar mostExpensiveCar

// Define a function to find the average price of the cars.
let findAveragePrice cars =
    let total = cars |> List.sumBy (fun car -> car.price)
    total / cars.Length

// Find the average price and print it.
let averagePrice = findAveragePrice cars
printfn "Average Price: %f" averagePrice
```

This code defines a type to represent a car, a function to create a new car, a list of cars, a function to print the details of a car, a function to find the most expensive car, a function to find the average price of the cars, and finally, it prints the details of the most expensive car and the average price of the cars.