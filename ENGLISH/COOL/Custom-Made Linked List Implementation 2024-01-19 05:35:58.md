```cool
class Node {
    String data;
    Node next;

    constructor(data: String, next: Node) {
        this.data = data;
        this.next = next;
    }
}

class LinkedList {
    Node first;
    Node last;
    Int size;

    constructor() {
        this.first = null;
        this.last = null;
        this.size = 0;
    }

    void add(String data) {
        Node newNode = new Node(data, null);
        if (this.size == 0) {
            this.first = newNode;
            this.last = newNode;
        } else {
            this.last.next = newNode;
            this.last = newNode;
        }
        this.size++;
    }

    String get(Int index) {
        if (index >= this.size || index < 0) {
            return null;
        }
        Node current = this.first;
        for (Int i = 0; i < index; i++) {
            current = current.next;
        }
        return current.data;
    }

    void remove(Int index) {
        if (index >= this.size || index < 0) {
            return;
        }
        Node current = this.first;
        Node previous = null;
        for (Int i = 0; i < index; i++) {
            previous = current;
            current = current.next;
        }
        if (previous == null) {
            this.first = current.next;
        } else {
            previous.next = current.next;
        }
        if (index == this.size - 1) {
            this.last = previous;
        }
        this.size--;
    }

    Int size() {
        return this.size;
    }

    Boolean isEmpty() {
        return this.size == 0;
    }

    String toString() {
        String result = "[";
        Node current = this.first;
        for (Int i = 0; i < this.size; i++) {
            result += current.data;
            if (i < this.size - 1) {
                result += ", ";
            }
            current = current.next;
        }
        result += "]";
        return result;
    }
}

class Main {
    static void main() {
        LinkedList list = new LinkedList();
        list.add("Hello");
        list.add("World");
        list.add("!");

        String first = list.get(0);
        String last = list.get(list.size() - 1);

        System.out.println("The first element is: " + first);
        System.out.println("The last element is: " + last);

        list.remove(1);
        String middle = list.get(1);

        System.out.println("The middle element is: " + middle);

        System.out.println("The size of the list is: " + list.size());

        Boolean empty = list.isEmpty();
        System.out.println("The list is empty: " + empty);

        String str = list.toString();
        System.out.println("The list as a string: " + str);
    }
}
```