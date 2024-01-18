```java
// Java program to efficiently implement hashing with chaining

import java.util.ArrayList;
import java.util.LinkedList;

class HashTable {

    private int SIZE = 16;
    private LinkedList<Integer>[] table;

    public HashTable() {
        table = new LinkedList[SIZE];
        for (int i = 0; i < SIZE; i++) {
            table[i] = new LinkedList<>();
        }
    }

    public void put(int key, int value) {
        int index = hash(key);
        LinkedList<Integer> list = table[index];
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i) == key) {
                list.set(i, value);
                return;
            }
        }
        list.add(value);
    }

    public int get(int key) {
        int index = hash(key);
        LinkedList<Integer> list = table[index];
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i) == key) {
                return list.get(i + 1);
            }
        }
        return -1;
    }

    public void remove(int key) {
        int index = hash(key);
        LinkedList<Integer> list = table[index];
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i) == key) {
                list.remove(i);
                return;
            }
        }
    }

    private int hash(int key) {
        return key % SIZE;
    }

    public static void main(String[] args) {
        HashTable ht = new HashTable();
        ht.put(1, 10);
        ht.put(2, 20);
        ht.put(3, 30);
        ht.put(4, 40);

        System.out.println(ht.get(1)); // 10
        System.out.println(ht.get(2)); // 20
        System.out.println(ht.get(3)); // 30
        System.out.println(ht.get(4)); // 40

        ht.remove(2);
        System.out.println(ht.get(2)); // -1
    }
}
```

Explanation:

This Java program implements a hash table with chaining to efficiently store and retrieve key-value pairs. Here's how it works:

1. **Hash Table Class**: The `HashTable` class defines the hash table data structure. It has an array of linked lists called `table` to store the key-value pairs.

2. **Constructor**: The constructor initializes the `table` array with a default size of 16 linked lists.

3. **`put` Method**: To insert a key-value pair, it calculates the index in the `table` array using the `hash` method and then adds the key and value to the linked list at that index. If the key already exists, it updates the value.

4. **`get` Method**: To retrieve the value associated with a key, it calculates the index in the `table` array and searches through the linked list at that index for the key. If found, it returns the value; otherwise, it returns -1.

5. **`remove` Method**: To delete a key-value pair, it calculates the index in the `table` array and searches through the linked list at that index to find the key. If found, it removes the key and its associated value from the linked list.

6. **`hash` Method**: The `hash` method is a simple hashing function that calculates the index in the `table` array based on the key. It uses the modulus operator (`%`) to map the key to a valid index within the range of the array.

7. **Main Method**: The `main` method creates an instance of the `HashTable` class and demonstrates the operations of inserting, getting, and removing key-value pairs. It prints the results to the console.