```cool
class Queue {
    private:
        int* arr;
        int front;
        int rear;
        int capacity;

    public:
        Queue(int capacity) {
            this->capacity = capacity;
            arr = new int[capacity];
            front = 0;
            rear = -1;
        }

        void enqueue(int data) {
            if (isFull()) {
                cout << "Queue is full" << endl;
                return;
            }
            rear = (rear + 1) % capacity;
            arr[rear] = data;
            if (front == -1) {
                front = rear;
            }
        }

        int dequeue() {
            if (isEmpty()) {
                cout << "Queue is empty" << endl;
                return -1;
            }
            int data = arr[front];
            front = (front + 1) % capacity;
            if (front == rear) {
                front = -1;
                rear = -1;
            }
            return data;
        }

        int front() {
            if (isEmpty()) {
                cout << "Queue is empty" << endl;
                return -1;
            }
            return arr[front];
        }

        bool isEmpty() {
            return (front == -1);
        }

        bool isFull() {
            return ((rear + 1) % capacity == front);
        }

        void printQueue() {
            if (isEmpty()) {
                cout << "Queue is empty" << endl;
                return;
            }
            int i = front;
            while (i != rear) {
                cout << arr[i] << " ";
                i = (i + 1) % capacity;
            }
            cout << arr[rear] << endl;
        }
};

int main() {
    Queue q(5);
    q.enqueue(1);
    q.enqueue(2);
    q.enqueue(3);
    q.enqueue(4);
    q.enqueue(5);
    q.printQueue();
    q.dequeue();
    q.printQueue();
    q.enqueue(6);
    q.printQueue();
    q.dequeue();
    q.dequeue();
    q.dequeue();
    q.dequeue();
    q.dequeue();
    q.printQueue();
    return 0;
}
```

This code implements a queue data structure in COOL. A queue is a FIFO (First-In-First-Out) data structure, meaning that the first element added to the queue is the first one to be removed.

The Queue class has a private attribute arr, which is an array of integers that stores the elements of the queue. It also has three private attributes: front, rear, and capacity. Front and rear are indices that point to the front and rear of the queue, respectively. Capacity is the maximum number of elements that the queue can hold.

The Queue class has several public methods:

* enqueue(data): Adds an element to the rear of the queue.
* dequeue(): Removes and returns the element at the front of the queue.
* front(): Returns the element at the front of the queue without removing it.
* isEmpty(): Checks if the queue is empty.
* isFull(): Checks if the queue is full.
* printQueue(): Prints the elements of the queue to the console.

The main() function creates a Queue object with a capacity of 5 and adds the numbers 1 to 5 to the queue. It then prints the queue, dequeues the first element, prints the queue again, enqueues the number 6, prints the queue again, and dequeues all the remaining elements from the queue. Finally, it prints the queue again to show that it is empty.

This code is a good example of how to implement a queue data structure in COOL. It is a complex code, but it is well-organized and easy to understand.