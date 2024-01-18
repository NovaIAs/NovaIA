```portugal
program BiggestAndSmallestNumber;

integer biggestNumber, smallestNumber, number;

begin
    biggestNumber := -1000000;
    smallestNumber := 1000000;

    for i := 1 to 10 do
    begin
        write("Enter a number: ");
        read(number);

        if number > biggestNumber then
            biggestNumber := number;
        end if;

        if number < smallestNumber then
            smallestNumber := number;
        end if;
    end for;

    write("The largest number is ", biggestNumber, " and the smallest number is ", smallestNumber, ".");
end.
```

This program asks the user to input 10 numbers. It then finds the biggest and smallest numbers among the 10 numbers.

The program starts by initializing the biggestNumber and smallestNumber variables to -1000000 and 1000000, respectively. This ensures that any number entered by the user will be greater than the biggestNumber variable and smaller than the smallestNumber variable.

The program then uses a for loop to iterate 10 times. In each iteration, it asks the user to enter a number and reads the input into the number variable.

The program then compares the number variable to the biggestNumber and smallestNumber variables. If the number is greater than the biggestNumber variable, the biggestNumber variable is updated to the number. If the number is smaller than the smallestNumber variable, the smallestNumber variable is updated to the number.

After all 10 numbers have been entered, the program prints the largest and smallest numbers to the console.

Here is an example of how the program would run:

```
Enter a number: 10
Enter a number: 20
Enter a number: 30
Enter a number: 40
Enter a number: 50
Enter a number: 60
Enter a number: 70
Enter a number: 80
Enter a number: 90
Enter a number: 100
The largest number is 100 and the smallest number is 10.
```