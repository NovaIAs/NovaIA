```java
import java.util.*;
import java.lang.*;
import java.math.*;

public class LargeComplexCode {

    // Define a custom exception for division by zero
    static class DivisionByZeroException extends Exception {
        public DivisionByZeroException() {
            super("Division by zero is not allowed.");
        }
    }

    // Define a custom linked list node class
    static class Node<T> {
        T data;
        Node<T> next;

        public Node(T data) {
            this.data = data;
            this.next = null;
        }
    }

    // Implement a custom linked list
    static class LinkedList<T> {
        Node<T> head;

        // Add a new element to the end of the list
        public void add(T data) {
            if (head == null) {
                head = new Node<>(data);
                return;
            }
            Node<T> current = head;
            while (current.next != null) {
                current = current.next;
            }
            current.next = new Node<>(data);
        }

        // Remove the first occurrence of an element from the list
        public void remove(T data) {
            if (head == null) {
                return;
            }
            if (head.data == data) {
                head = head.next;
                return;
            }
            Node<T> current = head;
            while (current.next != null) {
                if (current.next.data == data) {
                    current.next = current.next.next;
                    return;
                }
                current = current.next;
            }
        }

        // Check if the list contains an element
        public boolean contains(T data) {
            if (head == null) {
                return false;
            }
            Node<T> current = head;
            while (current != null) {
                if (current.data == data) {
                    return true;
                }
                current = current.next;
            }
            return false;
        }

        // Print the list
        public void print() {
            if (head == null) {
                System.out.println("List is empty.");
                return;
            }
            Node<T> current = head;
            while (current != null) {
                System.out.print(current.data + " ");
                current = current.next;
            }
            System.out.println();
        }
    }

    // Implement a custom stack using a linked list
    static class Stack<T> {
        LinkedList<T> list;

        public Stack() {
            list = new LinkedList<>();
        }

        // Push an element onto the stack
        public void push(T data) {
            list.add(data);
        }

        // Pop an element from the stack
        public T pop() {
            if (list.head == null) {
                return null;
            }
            T data = list.head.data;
            list.head = list.head.next;
            return data;
        }

        // Peek at the top element of the stack
        public T peek() {
            if (list.head == null) {
                return null;
            }
            return list.head.data;
        }

        // Check if the stack is empty
        public boolean isEmpty() {
            return list.head == null;
        }
    }

    // Implement a custom queue using a linked list
    static class Queue<T> {
        LinkedList<T> list;

        public Queue() {
            list = new LinkedList<>();
        }

        // Enqueue an element into the queue
        public void enqueue(T data) {
            list.add(data);
        }

        // Dequeue an element from the queue
        public T dequeue() {
            if (list.head == null) {
                return null;