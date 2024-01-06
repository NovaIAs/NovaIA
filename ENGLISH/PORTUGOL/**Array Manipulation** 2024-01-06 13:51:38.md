```portugal
PROGRAM ComplexCode;

PROCEDURE PrintArray(ARRAY arr);
    FOR i := 0 TO SIZE(arr) - 1 DO
        WRITE(arr[i]);
        IF i < SIZE(arr) - 1 THEN
            WRITE(", ");
        END IF;
    END FOR;
END PROCEDURE;

PROCEDURE FillArray(ARRAY arr);
    FOR i := 0 TO SIZE(arr) - 1 DO
        arr[i] := RANDOM(100);
    END FOR;
END PROCEDURE;

PROCEDURE SortArray(ARRAY arr);
    FOR i := 0 TO SIZE(arr) - 2 DO
        FOR j := i + 1 TO SIZE(arr) - 1 DO
            IF arr[i] > arr[j] THEN
                temp := arr[i];
                arr[i] := arr[j];
                arr[j] := temp;
            END IF;
        END FOR;
    END FOR;
END PROCEDURE;

VAR
    arr: ARRAY[0..9] OF INTEGER;
BEGIN
    FillArray(arr);
    WRITE("Unsorted array: ");
    PrintArray(arr);

    SortArray(arr);
    WRITE("Sorted array: ");
    PrintArray(arr);
END.
```

This code is a complex program written in PORTUGOL. It creates an array of integers, fills it with random numbers, sorts the array, and then prints the unsorted and sorted arrays.

The program starts by defining a procedure called `PrintArray`. This procedure takes an array as an argument and prints the elements of the array to the console.

Next, the program defines a procedure called `FillArray`. This procedure takes an array as an argument and fills it with random numbers.

The program then defines a procedure called `SortArray`. This procedure takes an array as an argument and sorts the elements of the array in ascending order.

The program then declares an array of integers called `arr` with a size of 10.

The program then calls the `FillArray` procedure to fill the `arr` array with random numbers.

The program then calls the `PrintArray` procedure to print the unsorted `arr` array to the console.

The program then calls the `SortArray` procedure to sort the `arr` array.

The program then calls the `PrintArray` procedure to print the sorted `arr` array to the console.

The output of the program will be something like this:

```
Unsorted array: 42, 17, 31, 88, 93, 50, 66, 77, 25, 12
Sorted array: 12, 17, 25, 31, 42, 50, 66, 77, 88, 93
```