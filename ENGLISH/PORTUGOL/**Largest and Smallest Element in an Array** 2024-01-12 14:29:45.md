```
// PROGRAM TO FIND THE LARGEST AND SMALLEST ELEMENT IN AN ARRAY OF INTEGERS

// DECLARE THE NECESSARY VARIABLES
INTEGER ARRAY[100], LARGEST, SMALLEST, I, N;

// GET THE SIZE OF THE ARRAY FROM THE USER
WRITE("ENTER THE SIZE OF THE ARRAY: ");
READ(N);

// GET THE ELEMENTS OF THE ARRAY FROM THE USER
FOR I = 1 TO N DO
  WRITE("ENTER ELEMENT ", I, ": ");
  READ(ARRAY[I]);
END FOR;

// INITIALIZE LARGEST AND SMALLEST WITH THE FIRST ELEMENT OF THE ARRAY
LARGEST = ARRAY[1];
SMALLEST = ARRAY[1];

// FIND THE LARGEST AND SMALLEST ELEMENTS IN THE ARRAY
FOR I = 2 TO N DO
  IF ARRAY[I] > LARGEST THEN
    LARGEST = ARRAY[I];
  END IF;
  IF ARRAY[I] < SMALLEST THEN
    SMALLEST = ARRAY[I];
  END IF;
END FOR;

// PRINT THE LARGEST AND SMALLEST ELEMENTS
WRITE("THE LARGEST ELEMENT IN THE ARRAY IS: ", LARGEST);
WRITE("THE SMALLEST ELEMENT IN THE ARRAY IS: ", SMALLEST);

```

**Explanation:**

This program finds the largest and smallest elements in an array of integers. It first declares the necessary variables, including an array to store the integers, variables to store the largest and smallest elements, and a variable to store the size of the array.

It then gets the size of the array from the user and the elements of the array from the user.

It then initializes the largest and smallest elements with the first element of the array.

It then uses a for loop to iterate through the array and find the largest and smallest elements.

Finally, it prints the largest and smallest elements.