```cool

class Stack {
    attribute:
        arr: Array(Int);
        top: Int;

    method:
        init(arr_size: Int) {
            arr := Array.new(arr_size);
            top := -1;
        }

        push(item: Int) {
            top := top + 1;
            arr[top] := item;
        }

        pop() -> Int {
            if top < 0 then
                return -1;  -- return special value to indicate stack is empty
            else
                item := arr[top];
                top := top - 1;
                return item;
            fi;
        }

        is_empty() -> Bool {
            return top < 0;
        }

        print_stack() {
            i := top;
            while i >= 0 loop
                print(arr[i]);
                i := i - 1;
            pool;
        }
}

class Queue {
    attribute:
        arr: Array(Int);
        front: Int;
        rear: Int;

    method:
        init(arr_size: Int) {
            arr := Array.new(arr_size);
            front := -1;
            rear := -1;
        }

        enqueue(item: Int) {
            if rear == -1 then  -- if queue is initially empty
                front := 0;
            fi;
            rear := rear + 1;
            arr[rear] := item;
        }

        dequeue() -> Int {
            if front == -1 then  -- if queue is empty
                return -1;  -- return special value to indicate queue is empty
            else
                item := arr[front];
                if front == rear then  -- if there is only one element in the queue
                    front := -1;
                    rear := -1;
                else
                    front := front + 1;
                fi;
                return item;
            fi;
        }

        is_empty() -> Bool {
            return front == -1;
        }

        print_queue() {
            i := front;
            while i <= rear loop
                print(arr[i]);
                i := i + 1;
            pool;
        }
}

class LinkedList {
    attribute:
        head: Node;
        tail: Node;

    method:
        init() {
            head := nil;
            tail := nil;
        }

        add_to_head(item: Int) {
            new_node := Node.new(item);
            if head is nil then
                head := new_node;
                tail := new_node;
            else
                new_node->next := head;
                head := new_node;
            fi;
        }

        add_to_tail(item: Int) {
            new_node := Node.new(item);
            if tail is nil then
                head := new_node;
                tail := new_node;
            else
                tail->next := new_node;
                tail := new_node;
            fi;
        }

        remove_from_head() -> Int {
            if head is nil then
                return -1;  -- return special value to indicate list is empty
            else
                item := head->data;
                if head == tail then  -- if there is only one element in the list
                    head := nil;
                    tail := nil;
                else
                    head := head->next;
                fi;
                return item;
            fi;
        }

        remove_from_tail() -> Int {
            if tail is nil then
                return -1;  -- return special value to indicate list is empty
            else
                item := tail->data;
                if head == tail then  -- if there is only one element in the list
                    head := nil;
                    tail := nil;
                else
                    current := head;
                    while current->next != tail loop
                        current := current->next;
                    pool;
                    current->next := nil;
                    tail := current;
                fi;
                return item;
            fi;
        }

        is_empty() -> Bool {
            return head is nil;
        }

        print_list() {
            current := head;
            while current is not nil loop
                print(current->data);
                current := current->next;
            pool;
        }
}

class Node {
    attribute:
        data: Int;
        next: Node;

    method:
        init(item: Int) {
            data := item;
            next := nil;
        }
}

class BST {
    attribute:
        root: Node;

    method:
        init() {
            root := nil;
        }

        insert(item: Int) {
            new_node := Node.new(item);
            if root is nil then
                root := new_node;
            else
                current := root;
                parent := nil;
                while current is not nil loop
                    if item < current->data then
                        parent := current;
                        current := current->left;
                    else
                        parent := current;
                        current := current->right;
                    fi;
                pool;
                if item < parent->data then
                    parent->left := new_node;
                else
                    parent->right := new_node;
                fi;
            fi;
        }

        search(item: Int) -> Bool {
            current := root;
            while current is not nil loop
                if item == current->data then
                    return true;
                else if item < current->data then
                    current := current->left;
                else
                    current := current->right;
                fi;
            pool;
            return false;
        }

        inorder_traversal() {
            inorder_traversal_helper(root);
        }

        inorder_traversal_helper(current: Node) {
            if current is nil then
                return;
            fi;
            inorder_traversal_helper(current->left);
            print(current->data);
            inorder_traversal_helper(current->right);
        }

        preorder_traversal() {
            preorder_traversal_helper(root);
        }

        preorder_traversal_helper(current: Node) {
            if current is nil then
                return;
            fi;
            print(current->data);
            preorder_traversal_helper(current->left);
            preorder_traversal_helper(current->right);
        }

        postorder_traversal() {
            postorder_traversal_help