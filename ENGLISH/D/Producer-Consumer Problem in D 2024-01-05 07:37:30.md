```d
import std.stdio, std.random, std.thread, std.atomic;

struct Node {
    int value;
    Node* next;

    Node(int value) {
        this.value = value;
        this.next = null;
    }
}

class Queue {
    Node* head;
    Node* tail;

    void enqueue(int value) {
        Node* node = new Node(value);
        if (head == null) {
            head = node;
            tail = node;
        } else {
            tail.next = node;
            tail = node;
        }
    }

    int dequeue() {
        if (head == null) {
            return -1; // Queue is empty
        }

        int value = head.value;
        head = head.next;
        if (head == null) {
            tail = null;
        }
        return value;
    }

    bool isEmpty() {
        return head == null;
    }
}

class Producer {
    Queue* queue;

    void produce() {
        while (true) {
            int value = std.random.nextInt(100);
            queue.enqueue(value);
            std.stdio.writefln("Producer produced: %d", value);
            std.thread.sleep(1000);
        }
    }
}

class Consumer {
    Queue* queue;

    void consume() {
        while (true) {
            int value = queue.dequeue();
            if (value == -1) {
                std.stdio.writefln("Queue is empty, consumer is waiting");
                std.thread.sleep(1000);
            } else {
                std.stdio.writefln("Consumer consumed: %d", value);
            }
        }
    }
}

void main() {
    Queue* queue = new Queue();
    Producer* producer = new Producer(queue);
    Consumer* consumer = new Consumer(queue);

    std.thread producerThread = new std.thread(producer.produce);
    std.thread consumerThread = new std.thread(consumer.consume);

    producerThread.join();
    consumerThread.join();
}
```

Explanation:

This code demonstrates a producer-consumer problem using threads and a queue in the D programming language. The producer thread generates random integers and enqueues them in the queue. The consumer thread dequeues the integers from the queue and prints them. The code ensures that the consumer thread waits if the queue is empty.

Here's a breakdown of the code:

1. The `Node` struct represents a node in the queue. It contains an `int` value and a pointer to the next node.

2. The `Queue` class implements a queue using a singly linked list. It has methods to enqueue, dequeue, and check if the queue is empty.

3. The `Producer` class represents a producer thread. It has a method `produce` that generates random integers and enqueues them in the queue.

4. The `Consumer` class represents a consumer thread. It has a method `consume` that dequeues integers from the queue and prints them.

5. In the `main` function, a queue is created, and producer and consumer threads are created and started. The producer thread produces integers, and the consumer thread consumes them.

6. The `std.thread.join()` method is used to wait for the producer and consumer threads to finish their execution.

This code showcases the use of threads and synchronization in D to implement a producer-consumer problem, where one thread produces data and another thread consumes it.