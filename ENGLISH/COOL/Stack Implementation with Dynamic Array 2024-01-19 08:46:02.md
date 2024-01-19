```cool
class Stack {
    private:
        int *arr;
        int top;
        int capacity;

    public:
        Stack(int size) {
            arr = new int[size];
            top = -1;
            capacity = size;
        }

        void push(int x) {
            if (top == capacity - 1) {
                throw std::overflow_error("Stack is full");
            }
            arr[++top] = x;
        }

        int pop() {
            if (top == -1) {
                throw std::underflow_error("Stack is empty");
            }
            return arr[top--];
        }

        int peek() {
            if (top == -1) {
                throw std::underflow_error("Stack is empty");
            }
            return arr[top];
        }

        bool isEmpty() {
            return top == -1;
        }

        int size() {
            return top + 1;
        }
};

int main() {
    Stack stack(5);

    stack.push(1);
    stack.push(2);
    stack.push(3);

    std::cout << stack.peek() << std::endl; // 3

    stack.pop();

    std::cout << stack.size() << std::endl; // 2

    stack.pop();
    stack.pop();

    std::cout << stack.isEmpty() << std::endl; // 1

    return 0;
}
```

Explanation:

The code implements a stack data structure using a dynamic array and provides a simple interface for pushing, popping, and checking for isEmpty. Here's an explanation:

1. **Class Definition:**

   - `Stack` Class:
     - Private members:
       - `*arr`: Pointer to an array of integers used to store the stack data.
       - `top`: Index of the top element in the array.
       - `capacity`: Capacity of the stack.
     - Public members:
       - Constructor `Stack(int size)`: Initializes the stack with a given size and sets `top` to -1.
       - `push(int x)`: Pushes an element `x` onto the stack.
       - `pop()`: Pops and returns the element at the top of the stack.
       - `peek()`: Returns the element at the top of the stack without removing it.
       - `isEmpty()`: Checks if the stack is empty.
       - `size()`: Returns the number of elements in the stack.

2. **Main Function:**

   - Creates a stack of size 5.
   - Pushes 1, 2, and 3 onto the stack.
   - Peeks at the top element (3) and prints it.
   - Pops an element (3) from the stack.
   - Prints the size of the stack (2).
   - Pops two more elements (2 and 1) from the stack.
   - Checks if the stack is empty (true) and prints the result.

This code demonstrates the basic operations of a stack data structure and provides a foundation for implementing more complex algorithms and data structures.