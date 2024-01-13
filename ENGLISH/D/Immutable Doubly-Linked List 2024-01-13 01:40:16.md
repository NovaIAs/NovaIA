```d
import std.stdio, std.terminal, std.traits, std.typecons, std.typecons.integer;

// The following are all types defined to help with the implementation of our
// doubly-linked list.
typedef node = immutable struct {
    immutable T value;
    immutable node* prev = null;
    immutable node* next = null;
};

template ImmutableList(T) struct ImmutableList {
    immutable node* head = null;
    immutable node* tail = null;

    // Create a new empty list.
    this() pure nothrow {}

    // Create a new list containing the given value.
    this(T value) pure nothrow {
        // Allocate a new node for the value.
        auto* n = new node!value;

        // Set the head and tail of the list to the new node.
        head = n;
        tail = n;
    }

    // Create a new list containing the given values.
    this(T[] values) pure nothrow {
        // Create an empty list.
        this();

        // Iterate over the values and add them to the list.
        foreach (i, value; values)
            append!(value);
    }

    // Return the number of elements in the list.
    immutable size_t length() pure nothrow {
        immutable size_t len = 0;

        // Iterate over the list and count the number of elements.
        immutable node* n = head;
        while (n != null) {
            len++;
            n = n.next;
        }

        return len;
    }

    // Check if the list is empty.
    immutable bool empty() pure nothrow {
        return head == null;
    }

    // Return the first element of the list.
    immutable T first() pure nothrow {
        if (head == null)
            throw new RangeError("List is empty.");

        return head.value;
    }

    // Return the last element of the list.
    immutable T last() pure nothrow {
        if (tail == null)
            throw new RangeError("List is empty.");

        return tail.value;
    }

    // Get the element at the given index.
    immutable T get(immutable size_t index) pure nothrow {
        if (index >= length())
            throw new RangeError("Index out of bounds.");

        // Iterate over the list until we reach the given index.
        immutable node* n = head;
        while (index > 0) {
            n = n.next;
            index--;
        }

        return n.value;
    }

    // Append a new value to the end of the list.
    immutable ImmutableList append!(T value) pure nothrow {
        // Allocate a new node for the value.
        auto* n = new node!value;

        // If the list is empty, set the head and tail to the new node.
        if (empty()) {
            head = n;
            tail = n;
        }
        // Otherwise, append the new node to the end of the list.
        else {
            tail.next = n;
            n.prev = tail;
            tail = n;
        }

        return this;
    }

    // Insert a new value at the given index.
    immutable ImmutableList insert!(immutable size_t index, T value) pure nothrow {
        if (index > length())
            throw new RangeError("Index out of bounds.");

        // If the list is empty, or the index is 0, prepend the new value to the
        // list.
        if (empty() || index == 0)
            return prepend!(value);

        // Otherwise, find the node at the given index and insert the new node
        // before it.
        immutable node* n = head;
        while (index > 0) {
            n = n.next;
            index--;
        }

        auto* new_node = new node!value;
        new_node.prev = n.prev;
        new_node.next = n;
        n.prev.next = new_node;
        n.prev = new_node;

        return this;
    }

    // Prepend a new value to the beginning of the list.
    immutable ImmutableList prepend!(T value) pure nothrow {
        // Allocate a new node for the value.
        auto* n = new node!value;

        // If the list is empty, set the head and tail to the new node.
        if (empty()) {
            head = n;
            tail = n;
        }
        // Otherwise, prepend the new node to the beginning of the list.
        else {
            n.next = head;
            head.prev = n;
            head = n;
        }

        return this;
    }

    // Remove the first element of the list.
    immutable ImmutableList removeFirst() pure nothrow {
        if (empty())
            throw new RangeError("List is empty.");

        // If the list has only one element, set the head and tail to null.
        if (head == tail) {
            head = null;
            tail = null;
        }
        // Otherwise, remove the first element from the list.
        else {
            head = head.next;
            head.prev = null;
        }

        return this;
    }

    // Remove the last element of the list.
    immutable ImmutableList removeLast() pure nothrow {
        if (empty())
            throw new RangeError("List is empty.");

        // If the list has only one element, set the head and tail to null.
        if (head == tail) {
            head = null;
            tail = null;
        }
        // Otherwise, remove the last element from the list.
        else {
            tail = tail.prev;
            tail.next = null;
        }

        return this;
    }

    // Remove the element at the given index.
    immutable ImmutableList removeAt(immutable size_t index) pure nothrow {
        if (index >= length())
            throw new RangeError("Index out of bounds.");

        // If the list is empty, or the index is 0, remove the first element from
        // the list.
        if (empty() || index == 0)
            return removeFirst();

        // If the index is the last element of the list, remove the last element
        // from the list.
        if (index == length() - 1)
            return removeLast();

        // Otherwise, find the node at the given index and remove it from the list.
        immutable node* n = head;
        while (index > 0) {
            n = n.next;
            index--;
        }

        n.prev.next = n.next;
        n.next.prev = n.prev;

        return this;
    }

    // Remove all occurrences of the given value from the list.
    immutable ImmutableList removeAll(T value) pure nothrow {
        immutable node* n = head;

        // Iterate over the list and remove all occurrences of the given value.
        while (n != null) {
            if (n.value == value) {
                // If the node is the head of the list, remove the first element.
                if (n == head)
                    removeFirst();
                // If the node is the tail of the list, remove the last element.
                else if (n == tail)
                    removeLast();
                // Otherwise, remove the node from the list.
                else {
                    n.prev.next = n.next;
                    n.next.prev = n.prev;
                }

                // Move to the next node.
                n = n.next;
            }
            else {
                // Move to the next node.
                n = n.next;
            }
        }

        return this;
    }

    // Reverse the list.
    immutable ImmutableList reverse() pure nothrow {
        // Swap the head and tail of the list.
        auto* temp = head;
        head = tail;
        tail = temp;

        // Reverse the links between the nodes.
        immutable node* n = head;
        while (n != null) {
            auto* temp = n.prev;
            n.prev = n.next;
            n.next = temp;

            // Move to the next node.
            n = n.prev;
        }

        return this;
    }

    // Sort the list in ascending order.
    immutable ImmutableList sort() pure nothrow {
        // If the list is empty, return an empty list.
        if (empty())
            return ImmutableList();

        // Create a new list to store the sorted elements.
        immutable ImmutableList sorted;

        while (this !empty()) {
            // Find the smallest element in the list.
            immutable node*