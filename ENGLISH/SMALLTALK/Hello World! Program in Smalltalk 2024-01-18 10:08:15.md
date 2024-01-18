```smalltalk
"Hello World! Program in Smalltalk"

"Define a class called 'HelloWorld'"
Object subclass: #HelloWorld
    instanceVariableNames: ''

"Define a method called 'sayHello' for the 'HelloWorld' class"
HelloWorld class addMethod: #sayHello
    [
        "Print 'Hello World!' to the console"
        Smalltalk at: #stdout putLine: 'Hello World!'
    ]

"Create an instance of the 'HelloWorld' class"
helloWorld := HelloWorld new

"Call the 'sayHello' method on the 'helloWorld' instance"
helloWorld sayHello
```

**Explanation:**

1. "Object subclass: #HelloWorld instanceVariableNames: ''"

    * This line defines a new class called 'HelloWorld' that inherits from the 'Object' class.
    * 'instanceVariableNames: ''` specifies that the 'HelloWorld' class does not have any instance variables.

2. "HelloWorld class addMethod: #sayHello [ Smalltalk at: #stdout putLine: 'Hello World!' ]"

    * This line adds a method called 'sayHello' to the 'HelloWorld' class.
    * The method body is a block of code that prints 'Hello World!' to the console using the 'putLine:' method of the 'stdout' stream.

3. "helloWorld := HelloWorld new"

    * This line creates a new instance of the 'HelloWorld' class and assigns it to the variable 'helloWorld'.

4. "helloWorld sayHello"

    * This line calls the 'sayHello' method on the 'helloWorld' instance, which prints 'Hello World!' to the console.