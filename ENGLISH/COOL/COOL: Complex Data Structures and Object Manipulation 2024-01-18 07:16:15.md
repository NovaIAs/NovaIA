```cool
class ComplexObject {
  -- Public fields
  field1: String;
  field2: Int;
  field3: Bool;

  -- Private fields
  private field4: List[String];
  private field5: Int[100];
  private field6: Dict[String, Int];

  -- Constructor
  new(field1: String, field2: Int, field3: Bool) {
    self.field1 := field1;
    self.field2 := field2;
    self.field3 := field3;
    self.field4 := List[String].new();
    self.field5 := Int[100].new();
    self.field6 := Dict[String, Int].new();
  }

  -- Methods
  method add_to_list(item: String) {
    self.field4 := self.field4 + [item];
  }

  method set_array_value(index: Int, value: Int) {
    self.field5[index] := value;
  }

  method add_to_dict(key: String, value: Int) {
    self.field6[key] := value;
  }

  method print_object() {
    Console.println("Field1: " + self.field1);
    Console.println("Field2: " + self.field2.toString());
    Console.println("Field3: " + self.field3.toString());

    Console.println("Field4:");
    for (item in self.field4) {
      Console.println(" - " + item);
    }

    Console.println("Field5:");
    for (index in 0..99) {
      Console.println(" - Index " + index.toString() + ": " + self.field5[index].toString());
    }

    Console.println("Field6:");
    for (key in self.field6.keys()) {
      Console.println(" - " + key + ": " + self.field6[key].toString());
    }
  }
}

-- Create an instance of the ComplexObject class
my_object := ComplexObject.new("This is a string", 42, true);

-- Add some items to the list
my_object.add_to_list("Item 1");
my_object.add_to_list("Item 2");
my_object.add_to_list("Item 3");

-- Set a value in the array
my_object.set_array_value(5, 100);

-- Add some key-value pairs to the dictionary
my_object.add_to_dict("Key 1", 200);
my_object.add_to_dict("Key 2", 300);

-- Print the object's fields
my_object.print_object();
```

This code defines a class called `ComplexObject` that contains a variety of fields, including public fields, private fields, and methods. The constructor method is used to initialize the object's fields. The methods allow the user to add items to the list, set values in the array, and add key-value pairs to the dictionary. The `print_object()` method is used to print the object's fields.

To use this code, you would first need to create an instance of the `ComplexObject` class. You can then use the methods to add items to the list, set values in the array, and add key-value pairs to the dictionary. Finally, you can use the `print_object()` method to print the object's fields.

This code is complex because it demonstrates a variety of features of the COOL language, including classes, fields, methods, and dictionaries. It also shows how to use loops and conditional statements to control the flow of execution.