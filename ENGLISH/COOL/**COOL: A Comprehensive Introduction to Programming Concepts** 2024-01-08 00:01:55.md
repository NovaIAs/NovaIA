```cool
class Main inherits IO {
    main() : Object {
        let number: Int <- 10;
        let arr: Array[6] <- [1, 2, 3, 4, 5, 6];
        let str: String <- "Hello World!";

        if (number > 5) then {
            out_string("The number is greater than 5.\n");
        } else {
            out_string("The number is less than or equal to 5.\n");
        }

        for i in 0 to 5 do {
            out_int(arr[i]);
        }

        out_string("\n");

        while (number > 0) do {
            number <- number - 1;
            out_int(number);
        }

        out_string("\n");

        let new_arr: Array[6] <- [7, 8, 9, 10, 11, 12];

        for i in 0 to 5 do {
            arr[i] <- new_arr[i];
        }

        for i in 0 to 5 do {
            out_int(arr[i]);
        }

        out_string("\n");

        return 0;
    }
}
```

Explanation:

The code defines a class `Main` that inherits from the built-in class `IO`, which provides input and output functionality. The `main` method is the entry point of the program.

It initializes several variables:

- `number` is an integer variable initialized to 10.
- `arr` is an array of integers with six elements, initialized to the values [1, 2, 3, 4, 5, 6].
- `str` is a string variable initialized to "Hello World!".

The code then uses conditional statements (if-else) to check if the value of `number` is greater than 5. If it is, it prints a message to the console using `out_string`. Otherwise, it prints a different message.

Next, it uses a `for` loop to iterate over the array `arr` and print each element to the console using `out_int`.

After that, it uses a `while` loop to decrement the value of `number` until it reaches 0. Inside the loop, it prints the current value of `number` to the console.

The code then defines a new array `new_arr` with six elements, initialized to the values [7, 8, 9, 10, 11, 12].

It uses another `for` loop to copy the elements of `new_arr` into the array `arr`.

Finally, it uses another `for` loop to print the elements of `arr` to the console.

The code returns 0 at the end of the `main` method to indicate successful execution.