```objective-c
#import <Foundation/Foundation.h>

// Define a block that takes an integer and returns a string.
NSString *(^formatNumber)(NSInteger) = ^(NSInteger number) {
    return [NSString stringWithFormat:@"%ld", number];
};

// Define a block that takes a string and returns an integer.
NSInteger (^parseNumber)(NSString *) = ^(NSString *string) {
    return [string integerValue];
};

// Define a block that takes an array of integers and returns the sum of the integers.
NSInteger (^sumIntegers)(NSArray *numbers) = ^(NSArray *numbers) {
    NSInteger sum = 0;
    for (NSInteger number in numbers) {
        sum += number;
    }
    return sum;
};

// Define a block that takes a string and returns an array of strings.
NSArray *(^splitString)(NSString *, NSString *) = ^(NSString *string, NSString *separator) {
    return [string componentsSeparatedByString:separator];
};

// Define a block that takes a string and returns a dictionary of key-value pairs.
NSDictionary *(^parseJSON)(NSString *) = ^(NSString *jsonString) {
    NSData *jsonData = [jsonString dataUsingEncoding:NSUTF8StringEncoding];
    return [NSJSONSerialization JSONObjectWithData:jsonData options:0 error:nil];
};

// Define a block that takes a dictionary of key-value pairs and returns a JSON string.
NSString *(^stringifyJSON)(NSDictionary *) = ^(NSDictionary *dictionary) {
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:dictionary options:0 error:nil];
    return [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
};

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create an array of integers.
        NSArray *numbers = @[@1, @2, @3, @4, @5];

        // Use the sumIntegers block to calculate the sum of the integers in the array.
        NSInteger sum = sumIntegers(numbers);

        // Log the sum to the console.
        NSLog(@"The sum of the numbers is %ld", sum);

        // Create a string.
        NSString *string = @"Hello, world!";

        // Use the splitString block to split the string into an array of strings.
        NSArray *words = splitString(string, @" ");

        // Log the words to the console.
        NSLog(@"The words in the string are %@", words);

        // Create a dictionary of key-value pairs.
        NSDictionary *dictionary = @{
            @"name": @"John Doe",
            @"age": @30,
            @"city": @"New York"
        };

        // Use the stringifyJSON block to convert the dictionary to a JSON string.
        NSString *jsonString = stringifyJSON(dictionary);

        // Log the JSON string to the console.
        NSLog(@"The JSON string is %@", jsonString);

        // Use the parseJSON block to convert the JSON string back into a dictionary.
        NSDictionary *parsedDictionary = parseJSON(jsonString);

        // Log the dictionary to the console.
        NSLog(@"The parsed dictionary is %@", parsedDictionary);
    }
    return 0;
}
```

This code is a complex and differentiated Objective-C program that demonstrates the use of blocks. It defines several blocks that perform various tasks, such as formatting and parsing numbers, splitting strings, and converting JSON data to and from dictionaries. The program also uses these blocks to perform various operations on data, such as calculating the sum of an array of integers and converting a dictionary to a JSON string.

Here is a detailed explanation of the code:

1. The program starts by importing the Foundation framework, which is the core framework of the Cocoa Touch SDK and provides the basic functionality for working with Objective-C objects.

2. The program defines several blocks using the `^` operator. Blocks are self-contained code blocks that can be passed as arguments to methods or functions. They are similar to anonymous functions in other programming languages.

3. The first block, `formatNumber`, takes an integer as an argument and returns a string representation of the integer.

4. The second block, `parseNumber`, takes a string as an argument and returns an integer representation of the string.

5. The third block, `sumIntegers`, takes an array of integers as an argument and returns the sum of the integers in the array.

6. The fourth block, `splitString`, takes a string and a separator as arguments and returns an array of strings that are separated by the separator.

7. The fifth block, `parseJSON`, takes a JSON string as an argument and returns a dictionary of key-value pairs that represents the JSON data.

8. The sixth block, `stringifyJSON`, takes a dictionary of key-value pairs as an argument and returns a JSON string that represents the dictionary.

9. The main function is the entry point of the program. It creates an array of integers, calculates the sum of the integers using the `sumIntegers` block, and logs the result to the console.

10. The main function also creates a string, splits the string into an array of words using the `splitString` block, and logs the words to the console.

11. The main function creates a dictionary of key-value pairs, converts the dictionary to a JSON string using the `stringifyJSON` block, and logs the JSON string to the console.

12. Finally, the main function converts the JSON string back into a dictionary using the `parseJSON` block and logs the dictionary to the console.

This program demonstrates the power and flexibility of blocks in Objective-C. Blocks can be used to perform a wide variety of tasks, and they can be passed as arguments to methods or functions, which makes them very versatile.