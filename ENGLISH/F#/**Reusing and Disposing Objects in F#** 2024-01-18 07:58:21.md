```f#

// Define a custom attribute to mark classes as being reusable
[<assembly: Reusable>]
type ReusableAttribute() =
    inherit Attribute()

// Define a class that implements the IDisposable interface
type DisposableClass() =
    mutable isDisposed = false

    // Implement IDisposable.Dispose
    member this.Dispose() =
        // Check if the object has already been disposed
        if not this.isDisposed then
            // Clean up resources
            Console.WriteLine("Disposing object")
            this.isDisposed <- true

// Define a class that uses the DisposableClass
type UsingClass() =
    mutable disposable = null

    // Constructor
    new (disposable: DisposableClass) =
        this.disposable <- disposable

    // Destructor
    override this.Finalize() =
        // Check if the disposable object has been created
        if this.disposable <> null then
            // Dispose the object
            this.disposable.Dispose()

// Define a class that demonstrates how to use the Reusable and Disposable classes
type MainClass() =
    let mutable reusableObject = null
    let mutable usingObject = null

    // Define a method to create and use reusable objects
    member this.CreateAndUseReusableObject() =
        // Create a new reusable object
        reusableObject <- new ReusableClass()

        // Use the reusable object
        Console.WriteLine("Using reusable object")
        reusableObject.DoSomething()

    // Define a method to create and use disposable objects
    member this.CreateAndUseDisposableObject() =
        // Create a new disposable object
        usingObject <- new UsingClass(new DisposableClass())

        // Use the disposable object
        Console.WriteLine("Using disposable object")
        usingObject.DoSomething()

    // Define a finalizer to clean up resources when the object is destroyed
    override this.Finalize() =
        // Check if the reusable object has been created
        if reusableObject <> null then
            // Dispose the reusable object
            reusableObject.Dispose()

        // Check if the disposable object has been created
        if usingObject <> null then
            // Dispose the disposable object
            usingObject.Dispose()

// Create an instance of the MainClass
let mainClass = new MainClass()

// Call the methods to create and use reusable and disposable objects
mainClass.CreateAndUseReusableObject()
mainClass.CreateAndUseDisposableObject()

// Force the garbage collector to run, which will call the finalizers for the MainClass instance and its child objects
GC.Collect()
GC.WaitForPendingFinalizers()
```

This code demonstrates how to create and use reusable and disposable classes in F#.

**Reusable Class**

The `ReusableClass` class is marked with the `Reusable` attribute. This attribute indicates that the class is intended to be reused by multiple clients. The class has a method called `DoSomething()` that can be used by clients to perform some operation.

**Disposable Class**

The `DisposableClass` class implements the `IDisposable` interface. This interface has a `Dispose()` method that is called when the object is no longer needed. The `Dispose()` method in this class cleans up any resources that were allocated by the object.

**Using Class**

The `UsingClass` class demonstrates how to use the `DisposableClass` class. The class has a constructor that takes a `DisposableClass` object as an argument. The constructor stores the `DisposableClass` object in a field. The class also has a `DoSomething()` method that can be used by clients to perform some operation.

**Main Class**

The `MainClass` class demonstrates how to use the `ReusableClass` and `DisposableClass` classes. The class has two methods: `CreateAndUseReusableObject()` and `CreateAndUseDisposableObject()`. The `CreateAndUseReusableObject()` method creates a `ReusableClass` object and uses it to perform some operation. The `CreateAndUseDisposableObject()` method creates a `UsingClass` object and uses it to perform some operation.

The `MainClass` class also has a finalizer that is called when the object is destroyed. The finalizer cleans up any resources that were allocated by the object.

**Usage**

The `mainClass` variable is created and the `CreateAndUseReusableObject()` and `CreateAndUseDisposableObject()` methods are called. The garbage collector is then forced to run, which calls the finalizers for the `MainClass` instance and its child objects.

This code demonstrates how to create and use reusable and disposable classes in F#. Reusable classes can be used by multiple clients, while disposable classes can be used to clean up resources when they are no longer needed.