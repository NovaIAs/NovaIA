```objective-c
// Import necessary frameworks
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Define constants and typedefs
const int kMaxNumberOfItems = 10;
typedef struct {
    int value;
    char *name;
} Item;

// Declare global variables
Item items[kMaxNumberOfItems];
int numberOfItems = 0;

// Function to add an item to the array
void addItem(int value, char *name) {
    if (numberOfItems < kMaxNumberOfItems) {
        items[numberOfItems].value = value;
        items[numberOfItems].name = strdup(name);
        numberOfItems++;
    }
}

// Function to remove an item from the array
void removeItem(int index) {
    if (index >= 0 && index < numberOfItems) {
        for (int i = index; i < numberOfItems - 1; i++) {
            items[i] = items[i + 1];
        }
        numberOfItems--;
    }
}

// Function to find an item in the array
int findItem(char *name) {
    for (int i = 0; i < numberOfItems; i++) {
        if (strcmp(items[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

// Function to print the array
void printArray() {
    printf("Array contents:\n");
    for (int i = 0; i < numberOfItems; i++) {
        printf("%d %s\n", items[i].value, items[i].name);
    }
}

// Main function
int main(int argc, char *argv[]) {
    @autoreleasepool {
        // Initialize the array
        addItem(1, "Item 1");
        addItem(2, "Item 2");
        addItem(3, "Item 3");

        // Print the array
        printArray();

        // Remove an item from the array
        removeItem(1);

        // Find an item in the array
        int index = findItem("Item 3");

        // Print the array again
        printArray();

        // Release the memory allocated for the item names
        for (int i = 0; i < numberOfItems; i++) {
            free(items[i].name);
        }
    }
    return 0;
}
```

Explanation:

1. Import necessary frameworks: This line imports the Foundation and UIKit frameworks, which are essential for working with Objective-C.
2. Define constants and typedefs: Here, we define a constant `kMaxNumberOfItems` to specify the maximum number of items that can be stored in the array. We also define a `struct` called `Item` to represent each item in the array, consisting of a value (an integer) and a name (a character array).
3. Declare global variables: We declare an array called `items` to store the items. We also declare a variable `numberOfItems` to keep track of the number of items in the array.
4. Function to add an item to the array (`addItem`): This function takes two parameters: a value and a name. It checks if there is space in the array (less than `kMaxNumberOfItems`) and adds the new item to the array. It also increments the `numberOfItems` variable.
5. Function to remove an item from the array (`removeItem`): This function takes an index as a parameter. It checks if the index is valid (within the array bounds) and removes the item at that index by shifting the remaining items down. It also decrements the `numberOfItems` variable.
6. Function to find an item in the array (`findItem`): This function takes a name as a parameter. It searches through the array for an item with the given name and returns the index of that item, or -1 if the item is not found.
7. Function to print the array (`printArray`): This function prints the contents of the array to the console. It iterates through the array and prints the value and name of each item.
8. Main function (`main`): This is the entry point of the program. It initializes the array with some items, prints the array, removes an item, finds an item, and prints the array again. Finally, it releases the memory allocated for the item names.

This code demonstrates various programming concepts such as data structures (arrays and structs), functions, and memory management in Objective-C. It also includes error checking and input validation to ensure that the program behaves correctly.