```cool
class ADTPriorityQueue {
    root : Seq[Node]  # the binary heap

    add(item):
        # Add an item to the priority queue.
        # Assumes that the item is not already in the queue.
        if root == null then
            root := [Node(item)]
        else
            i := Size()
            Grow()
            root[i] := Node(item)
            BubbleUp(i)

    remove():
        # Remove and return the item with the highest priority.
        # Assumes that the priority queue is not empty.
        item := root[0].item
        root[0] := root[Size() - 1]
        root := root[1..Size() - 2]
        BubbleDown(0)
        return item

    change_priority(item, new_priority):
        # Change the priority of an item in the priority queue.
        # Assumes that the item is already in the queue.
        i := Find(item)
        if i == -1 then
            return false
        root[i].priority := new_priority
        if new_priority > root[i].parent_priority then
            BubbleUp(i)
        else
            BubbleDown(i)
        return true

    contains(item):
        # Return whether an item is in the priority queue.
        return Find(item) != -1

    Size():
        # Return the number of items in the priority queue.
        return Length(root)

    Grow():
        # Double the size of the binary heap.
        new_root := [null] * Size() * 2
        for i in 0..Size() - 1 do
            new_root[i] := root[i]
        root := new_root

    Find(item):
        # Return the index of an item in the binary heap.
        # Return -1 if the item is not in the heap.
        for i in 0..Size() - 1 do
            if root[i].item == item then
                return i
        return -1

    BubbleUp(i):
        # Move an item up the binary heap until it is in its correct place.
        while i > 0 and root[i].priority > root[i div 2].priority do
            Swap(root[i], root[i div 2])
            i := i div 2

    BubbleDown(i):
        # Move an item down the binary heap until it is in its correct place.
        while i < Size() and (
            (i * 2 + 1 < Size() and root[i].priority < root[i * 2 + 1].priority) or
            (i * 2 + 2 < Size() and root[i].priority < root[i * 2 + 2].priority)
        ) do
            if root[i * 2 + 1].priority > root[i * 2 + 2].priority then
                Swap(root[i], root[i * 2 + 1])
                i := i * 2 + 1
            else
                Swap(root[i], root[i * 2 + 2])
                i := i * 2 + 2

    Swap(a, b):
        # Swap two elements in the binary heap.
        temp := a
        a := b
        b := temp
}

class Node {
    item : Any
    priority : Int
    parent_priority : Int

    constructor(item, priority):
        self.item := item
        self.priority := priority
        self.parent_priority := priority
}
```

This code implements a priority queue data structure using a binary heap.
The priority queue class has methods for adding, removing, and changing the priority of items, as well as checking if an item is in the queue and getting the size of the queue.
The node class represents a node in the binary heap, and has fields for the item, the priority, and the priority of the parent node.
The bubble up and bubble down methods are used to maintain the heap property, which is that each node's priority is greater than or equal to its children's priorities.
The swap method is used to swap two nodes in the binary heap.