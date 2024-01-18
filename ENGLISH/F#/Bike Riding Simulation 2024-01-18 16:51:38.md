```f#
module Program =

    // Define custom types for Bike and Rider
    type Bike () =
        { Speed : int
          Gears : int }

    type Rider =
        { Name : string
          Bike : Bike }

    // Initialize a Bike and a Rider object
    let bike = Bike(0, 5)
    let rider = Rider("John", bike)

    // Accelerate the bike by incrementing its speed
    let accelerateBike rider =
        { rider with Bike = { rider.Bike with Speed = rider.Bike.Speed + 1 } }

    // Decelerate the bike by decrementing its speed
    let decelerateBike rider =
        { rider with Bike = { rider.Bike with Speed = rider.Bike.Speed - 1 } }

    // Change the bike's gear
    let changeGear rider gear =
        { rider with Bike = { rider.Bike with Gears = gear } }

    // Display the rider's name, bike's speed, and current gear
    let displayRiderInfo rider =
        printfn "Rider Name: %s\nBike Speed: %d\nCurrent Gear: %d"
              rider.Name
              rider.Bike.Speed
              rider.Bike.Gears


    // Main function to test the program
    [<EntryPoint>]
    let main argv =

        // Accelerate bike 5 times
        let riderAfter5Acceleration = accelerateBike rider |> accelerateBike |> accelerateBike |> accelerateBike |> accelerateBike

        // Decelerate bike 3 times
        let riderAfter3Deceleration = decelerateBike riderAfter5Acceleration |> decelerateBike |> decelerateBike

        // Change gear to 3
        let riderWithGear3 = changeGear riderAfter3Deceleration 3

        // Display the rider's information after all operations
        displayRiderInfo riderWithGear3

        0
```

This F# code showcases a more complex program involving custom types, functions, and a main function. It simulates a scenario where a Rider object rides a Bike object, and you can perform certain operations like accelerating, decelerating, and changing gears. The program provides a detailed explanation of each function and also displays the Rider's information after applying the operations.

Let's go through the code in detail:

1. Custom Types:
   - `Bike`: Represents a bicycle with properties like `Speed` and `Gears`.
   - `Rider`: Represents a person riding the bike, with properties like `Name` and `Bike`.

2. Initialization of Bike and Rider Objects:
   - An instance of `Bike` and `Rider` is created and assigned to variables `bike` and `rider`, respectively.

3. Accelerate and Decelerate Functions:
   - `accelerateBike` increases the bike's speed by 1.
   - `decelerateBike` decreases the bike's speed by 1.

4. Change Gear Function:
   - `changeGear` changes the bike's gear to the specified value.

5. Display Rider Information Function:
   - `displayRiderInfo` prints the rider's name, bike's speed, and current gear.

6. Main Function:
   - In the `main` function, various operations are performed on the `rider` object:
     - Accelerate the bike 5 times.
     - Decelerate the bike 3 times.
     - Change the bike's gear to 3.
     - Finally, display the rider's information after all these operations.

7. Running the Program:
   - When you run this program, it will output the updated information about the rider (John), including the bike's speed and current gear after performing the specified operations.

This code demonstrates the use of custom types, functions, and control flow in F# to represent a simplified biking scenario. It allows you to manipulate the bike's speed, gear, and display relevant information.