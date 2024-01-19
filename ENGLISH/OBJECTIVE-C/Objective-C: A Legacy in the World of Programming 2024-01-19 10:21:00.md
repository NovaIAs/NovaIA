Objective-C is not a programming language anymore. It is a merge of C and a small superset of Smalltalk-80 messaging syntax, developed by Brad Cox and Tom Love in the early 1980s.

Objective-C was the main programming language used by Apple for macOS and iOS operating systems until it was replaced by Swift in 2014.

It was a general-purpose, object-oriented programming language that allowed programmers to create applications.

The syntax was similar to C, but it also added a number of features, such as object-oriented programming, dynamic typing, and garbage collection.

Objective-C was popular for developing applications for the macOS and iOS operating systems, but it has been replaced by Swift as Apple's preferred language for these platforms.

Here is an example of an Objective-C program that prints “Hello, world!” to the console:

```objective-c
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSLog(@"Hello, world!");
    }
    return 0;
}
```

This program first imports the Foundation framework, which provides basic functionality for macOS and iOS applications.

The main function is the entry point of the program, and it takes two arguments: argc, which is the number of arguments passed to the program, and argv, which is an array of strings containing the arguments.

The @autoreleasepool block is used to manage memory for the program.

The NSLog function is used to print a message to the console.

The return 0; statement is used to indicate that the program exited successfully.

This is a very simple Objective-C program, but it demonstrates the basic syntax of the language.