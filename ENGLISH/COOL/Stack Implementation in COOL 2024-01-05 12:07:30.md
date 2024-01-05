```cool
class Stack {
  % Fields
  self : Stack;
  top : Int;
  array : String[]; % array of Strings
  % Constructor
  new(self: Stack, top: Int, array: String[]) {
    self.top := top;
    self.array := array;
  }
  % Methods
  push(self: Stack, s: String) {
    self.array[self.top] := s;
    self.top := self.top + 1;
  }
  pop(self: Stack) : String {
    self.top := self.top - 1;
    return self.array[self.top];
  }
  isEmpty(self: Stack) : Bool {
    return self.top = 0;
  }
  printStack(self: Stack) {
    while (not self.isEmpty()) loop
      printString(self.pop());
      printChar('\n');
    pool;
  }
}

class Main {
  % Main method
  main() {
    s : Stack;
    s := new Stack(0, 10);
    s.push("Hello");
    s.push("World");
    s.printStack();
  }
}
```

Explanation:

1. **Class Stack:** This class represents a stack data structure.

2. **Fields:**
   - **self:** Reference to the current object.
   - **top:** The index of the top element in the stack.
   - **array:** An array of strings to store the elements of the stack.

3. **Constructor:** The constructor initializes the stack with a given top index and an array of strings.

4. **Methods:**
   - **push:** Pushes a string onto the stack by adding it to the array at the top index and incrementing the top index.
   - **pop:** Removes and returns the top element of the stack by decrementing the top index and returning the value at the new top index.
   - **isEmpty:** Checks if the stack is empty by comparing the top index to 0.
   - **printStack:** Prints all the elements of the stack from top to bottom using a loop to pop and print each element.

5. **Class Main:** This class contains the main method.

6. **Main Method:**
   - **s : Stack:** Declares a variable `s` of type Stack.
   - **s := new Stack(0, 10):** Creates a new Stack object with a top index of 0 and an array of 10 strings.
   - **s.push("Hello"); s.push("World");:** Pushes the strings "Hello" and "World" onto the stack.
   - **s.printStack();:** Calls the printStack method to print the elements of the stack.