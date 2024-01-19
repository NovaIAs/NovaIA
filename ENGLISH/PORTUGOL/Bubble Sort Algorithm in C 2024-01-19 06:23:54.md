```portugol

#include <stdio.h>

int main() {
    // Define the size of the array
    int size = 10;

    // Create an array of integers
    int array[size];

    // Initialize the array with random values
    for (int i = 0; i < size; i++) {
        array[i] = rand() % 100;
    }

    // Print the original array
    printf("Original array:\n");
    for (int i = 0; i < size; i++) {
        printf("%d ", array[i]);
    }

    // Sort the array in ascending order
    for (int i = 0; i < size - 1; i++) {
        for (int j = i + 1; j < size; j++) {
            if (array[i] > array[j]) {
                // Swap the values of array[i] and array[j]
                int temp = array[i];
                array[i] = array[j];
                array[j] = temp;
            }
        }
    }

    // Print the sorted array
    printf("\nSorted array:\n");
    for (int i = 0; i < size; i++) {
        printf("%d ", array[i]);
    }

    return 0;
}

```

This code is a program in C that sorts an array of integers in ascending order.

The program starts by defining the size of the array, which is 10 in this case. Then, it creates an array of integers of size 10.

The next step is to initialize the array with random values. This is done using the rand() function, which generates a random number between 0 and RAND_MAX. The modulus operator % is then used to generate a random number between 0 and 99.

The original array is then printed to the console using a for loop.

The next step is to sort the array in ascending order. This is done using the bubble sort algorithm. The bubble sort algorithm works by repeatedly comparing adjacent elements in the array and swapping them if they are in the wrong order. The algorithm continues until no more swaps are necessary.

The sorted array is then printed to the console using another for loop.

The program then exits, returning 0 to indicate that it ran successfully.