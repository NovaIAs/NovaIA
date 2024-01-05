```c#
// This program demonstrates the use of a class with properties, methods, and events.

// Define a delegate for the event.
public delegate void MyEventHandler(object sender, MyEventArgs e);

// Define the event arguments class.
public class MyEventArgs : EventArgs
{
    public string Message { get; set; }

    public MyEventArgs(string message)
    {
        Message = message;
    }
}

// Define the class.
public class MyClass
{
    // Define the event.
    public event MyEventHandler MyEvent;

    // Define the properties.
    public string Name { get; set; }
    public int Age { get; set; }

    // Define the methods.
    public void DoSomething()
    {
        // Raise the event.
        if (MyEvent != null)
        {
            MyEvent(this, new MyEventArgs("Something happened!"));
        }
    }
}

// Create an instance of the class.
MyClass myObject = new MyClass();

// Add an event handler to the event.
myObject.MyEvent += new MyEventHandler(MyEventHandlerMethod);

// Call the method to raise the event.
myObject.DoSomething();

// Define the event handler method.
private void MyEventHandlerMethod(object sender, MyEventArgs e)
{
    // Display the message.
    Console.WriteLine(e.Message);
}
```

This code defines a class called `MyClass` with properties, methods, and events. It also defines a delegate for the event, an event arguments class, and an event handler method. The `DoSomething()` method raises the event, which is handled by the `MyEventHandlerMethod()` method.

The following is a breakdown of the code:

* **Delegate:** The `MyEventHandler` delegate is defined to represent the event handler method. It takes two parameters: an object of type `object` and an object of type `MyEventArgs`.
* **Event Arguments Class:** The `MyEventArgs` class is defined to hold the data that is passed to the event handler method. It has a property called `Message` that is of type `string`.
* **Event:** The `MyEvent` event is defined in the `MyClass` class. It is of type `MyEventHandler`, which means that it can be handled by a method that takes two parameters: an object of type `object` and an object of type `MyEventArgs`.
* **Properties:** The `Name` and `Age` properties are defined in the `MyClass` class. They are of type `string` and `int`, respectively.
* **Methods:** The `DoSomething()` method is defined in the `MyClass` class. It raises the `MyEvent` event.
* **Event Handler Method:** The `MyEventHandlerMethod()` method is defined to handle the `MyEvent` event. It takes two parameters: an object of type `object` and an object of type `MyEventArgs`. It displays the message that is passed in the `MyEventArgs` object.

The following is an example of how to use the code:

```c#
// Create an instance of the class.
MyClass myObject = new MyClass();

// Add an event handler to the event.
myObject.MyEvent += new MyEventHandler(MyEventHandlerMethod);

// Call the method to raise the event.
myObject.DoSomething();

// Define the event handler method.
private void MyEventHandlerMethod(object sender, MyEventArgs e)
{
    // Display the message.
    Console.WriteLine(e.Message);
}
```

This code creates an instance of the `MyClass` class, adds an event handler to the `MyEvent` event, and then calls the `DoSomething()` method to raise the event. The event handler method is called and displays the message that is passed in the `MyEventArgs` object.