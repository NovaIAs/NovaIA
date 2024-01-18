```c#
// CustomAttributeExample.cs
// compile with: csc CustomAttributeExample.cs

using System;
using System.Linq;
using System.Reflection;

[AttributeUsage(AttributeTargets.Class | AttributeTargets.Constructor | AttributeTargets.Method)]
class ExampleAttribute : Attribute
{
    public string Description { get; set; }

    public ExampleAttribute(string description)
    {
        Description = description;
    }
}

[Example("This is an example class.")]
class ExampleClass
{
    [Example("This is an example constructor.")]
    public ExampleClass() { }

    [Example("This is an example method.")]
    public void ExampleMethod() { }
}

class Program
{
    static void Main()
    {
        // Get the type of the ExampleClass.
        Type exampleType = typeof(ExampleClass);

        // Get the custom attributes applied to the ExampleClass.
        object[] attributes = exampleType.GetCustomAttributes(typeof(ExampleAttribute), true);

        // Iterate over each attribute.
        foreach (ExampleAttribute attribute in attributes)
        {
            // Print the description of the attribute.
            Console.WriteLine(attribute.Description);
        }

        // Get the constructor of the ExampleClass.
        ConstructorInfo constructor = exampleType.GetConstructor(Type.EmptyTypes);

        // Get the custom attributes applied to the ExampleClass constructor.
        attributes = constructor.GetCustomAttributes(typeof(ExampleAttribute), true);

        // Iterate over each attribute.
        foreach (ExampleAttribute attribute in attributes)
        {
            // Print the description of the attribute.
            Console.WriteLine(attribute.Description);
        }

        // Get a method of the ExampleClass.
        MethodInfo method = exampleType.GetMethod("ExampleMethod");

        // Get the custom attributes applied to the ExampleClass method.
        attributes = method.GetCustomAttributes(typeof(ExampleAttribute), true);

        // Iterate over each attribute.
        foreach (ExampleAttribute attribute in attributes)
        {
            // Print the description of the attribute.
            Console.WriteLine(attribute.Description);
        }
    }
}

```

**Explanation:**
1. Create a custom attribute `ExampleAttribute` with a string property `Description` to store the description of the attribute.
2. Apply the `ExampleAttribute` attribute to a class, constructor, and method to add descriptions to them.
3. In the `Program` class, get the type of the `ExampleClass`.
4. Get the custom attributes applied to the class, constructor, and method using the `GetCustomAttributes` method.
5. Iterate over the attributes and print the description of each attribute.
6. Run the program to see the descriptions of the attributes applied to the class, constructor, and method.