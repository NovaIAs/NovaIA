```c++
// This program demonstrates the use of various advanced features in C++.
// It includes concepts such as templates, exception handling, and dynamic memory allocation.

#include <iostream>
#include <vector>
#include <memory>

using namespace std;

// Define a generic class template for a singly linked list node.
template <typename T>
struct Node {
    T data;
    Node<T>* next;

    Node(const T& value) : data(value), next(nullptr) {}
};

// Define a generic class template for a singly linked list.
template <typename T>
class LinkedList {
private:
    Node<T>* head;
    Node<T>* tail;
    size_t size;

public:
    // Constructor
    LinkedList() : head(nullptr), tail(nullptr), size(0) {}

    // Destructor
    ~LinkedList() {
        while (head != nullptr) {
            Node<T>* next = head->next;
            delete head;
            head = next;
        }
    }

    // Add a new node to the end of the list.
    void push_back(const T& value) {
        if (head == nullptr) {
            head = tail = new Node<T>(value);
        } else {
            tail->next = new Node<T>(value);
            tail = tail->next;
        }
        size++;
    }

    // Insert a new node at a specific position in the list.
    void insert(size_t index, const T& value) {
        if (index == 0) {
            push_front(value);
        } else if (index == size) {
            push_back(value);
        } else {
            Node<T>* new_node = new Node<T>(value);
            Node<T>* current = head;
            for (size_t i = 0; i < index - 1; i++) {
                current = current->next;
            }
            new_node->next = current->next;
            current->next = new_node;
            size++;
        }
    }

    // Remove a node from a specific position in the list.
    void remove(size_t index) {
        if (index == 0) {
            pop_front();
        } else if (index == size - 1) {
            pop_back();
        } else {
            Node<T>* current = head;
            for (size_t i = 0; i < index - 1; i++) {
                current = current->next;
            }
            Node<T>* node_to_remove = current->next;
            current->next = node_to_remove->next;
            delete node_to_remove;
            size--;
        }
    }

    // Get the value of the node at a specific position in the list.
    T& at(size_t index) {
        if (index >= size) {
            throw out_of_range("Index out of bounds");
        }
        Node<T>* current = head;
        for (size_t i = 0; i < index; i++) {
            current = current->next;
        }
        return current->data;
    }

    // Get the size of the list.
    size_t get_size() const {
        return size;
    }

    // Check if the list is empty.
    bool is_empty() const {
        return size == 0;
    }

    // Print the list to the console.
    void print() {
        Node<T>* current = head;
        while (current != nullptr) {
            cout << current->data << " ";
            current = current->next;
        }
        cout << endl;
    }

private:
    // Add a new node to the beginning of the list.
    void push_front(const T& value) {
        Node<T>* new_node = new Node<T>(value);
        new_node->next = head;
        head = new_node;
        if (tail == nullptr) {
            tail = head;
        }
        size++;
    }

    // Remove the node from the beginning of the list.
    void pop_front() {
        if (head == nullptr) {
            throw out_of_range("List is empty");
        }
        Node<T>* node_to_remove = head;
        head = head->next;
        if (head == nullptr) {
            tail = nullptr;
        }
        delete node_to_remove;
        size--;
    }

    // Remove the node from the end of the list.
    void pop_back() {
        if (tail == nullptr) {
            throw out_of_range("List is empty");
        }
        Node<T>* current = head;
        while (current->next != tail) {
            current = current->next;
        }
        delete tail;
        tail = current;
        tail->next = nullptr;
        if (tail == nullptr) {
            head = nullptr;
        }
        size--;
    }
};

// Define a custom exception class for list-related errors.
class ListException : public exception {
public:
    const char* what() const noexcept override {
        return "List error";
    }
};

// Main function
int main() {
    // Create a linked list of integers.
    LinkedList<int> list;

    // Add some values to the list.
    list.push_back(1);
    list.push_back(2);
    list.push_back(3);
    list.push_back(4);
    list.push_back(5);

    // Print the list.
    cout << "List: ";
    list.print();

    // Insert a value at a specific position.
    try {
        list.insert(2, 10);
        cout << "Inserted 10 at position 2" << endl;
    } catch (const ListException& e) {
        cout << "Error: " << e.what() << endl;
    }

    // Remove a value from a specific position.
    try {
        list.remove(3);
        cout << "Removed value at position 3" << endl;
    } catch (const ListException& e) {
        cout << "Error: " << e.what() << endl;
    }

    // Get the value at a specific position.
    try {
        int value = list.at(2);
        cout << "Value at position 2: " << value << endl;
    } catch (const out_of_range& e) {
        cout << "Error: " << e.what() << endl;
    }

    // Check if the list is empty.
    cout << "Is list empty? " << (list.is_empty() ? "Yes" : "No") << endl;

    // Get the size of the list.
    cout << "List size: " << list.get_size() << endl;

    // Print the list again.
    cout << "List: ";
    list.print();

    return 0;
}
```

This program implements a singly linked list class using a generic template. It demonstrates various operations on the list, including adding and removing nodes, inserting nodes at a specific position, getting the value at a specific position, checking if the list is empty, and getting the size of the list.

The program also demonstrates the use of exception handling to catch errors that may occur during these operations. For example, it catches the `out_of_range` exception when trying to access an invalid position in the list.

Additionally, the program demonstrates the use of dynamic memory allocation to allocate and deallocate nodes in the list. This allows the list to grow and shrink dynamically as needed.