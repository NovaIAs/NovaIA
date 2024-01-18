```cool
class Main inherits IO {
   main() : Void {
      return;
   }
}

class Array {
   elements: Array of Object;
   length: Int;

   constructor: (elements: Array of Object) -> Self {
      self := super.constructor();
      self.elements := elements;
      self.length := elements.length;
      return self;
   }

   get_: (index: Int) -> Object {
      if (index < 0 or index >= self.length) then {
         error("Index out of bounds");
      }
      return self.elements[index];
   }

   set_: (index: Int, value: Object) -> Void {
      if (index < 0 or index >= self.length) then {
         error("Index out of bounds");
      }
      self.elements[index] := value;
   }

   append: (value: Object) -> Void {
      self.elements := self.elements + [value];
      self.length := self.length + 1;
   }

   remove: (index: Int) -> Void {
      if (index < 0 or index >= self.length) then {
         error("Index out of bounds");
      }
      self.elements := self.elements[0:index-1] + self.elements[index+1:self.length];
      self.length := self.length - 1;
   }

   reverse: () -> Array {
      return Array.constructor(self.elements.reverse());
   }
}

class Stack inherits Array {
   constructor: () -> Self {
      super.constructor([]);
      return self;
   }

   push: (value: Object) -> Void {
      self.append(value);
   }

   pop: () -> Object {
      if (self.length = 0) then {
         error("Stack is empty");
      }
      return self.elements[self.length-1];
   }

   peek: () -> Object {
      if (self.length = 0) then {
         error("Stack is empty");
      }
      return self.elements[self.length-1];
   }
}

class Queue inherits Array {
   constructor: () -> Self {
      super.constructor([]);
      return self;
   }

   enqueue: (value: Object) -> Void {
      self.append(value);
   }

   dequeue: () -> Object {
      if (self.length = 0) then {
         error("Queue is empty");
      }
      return self.elements[0];
   }

   peek: () -> Object {
      if (self.length = 0) then {
         error("Queue is empty");
      }
      return self.elements[0];
   }
}

class BinaryTree {
   value: Object;
   left: BinaryTree;
   right: BinaryTree;

   constructor: (value: Object, left: BinaryTree, right: BinaryTree) -> Self {
      self := super.constructor();
      self.value := value;
      self.left := left;
      self.right := right;
      return self;
   }

   inorder: () -> Array {
      return self.left.inorder() + [self.value] + self.right.inorder();
   }

   preorder: () -> Array {
      return [self.value] + self.left.preorder() + self.right.preorder();
   }

   postorder: () -> Array {
      return self.left.postorder() + self.right.postorder() + [self.value];
   }
}

class LinkedList {
   value: Object;
   next: LinkedList;

   constructor: (value: Object, next: LinkedList) -> Self {
      self := super.constructor();
      self.value := value;
      self.next := next;
      return self;
   }

   reverse: () -> LinkedList {
      if (self.next = nil) then {
         return self;
      }
      reversedList := self.next.reverse();
      self.next.next := self;
      self.next := nil;
      return reversedList;
   }
}

class Graph {
   vertices: Array of Object;
   edges: Map of (Object, Object) to Int;

   constructor: (vertices: Array of Object, edges: Map of (Object, Object) to Int) -> Self {
      self := super.constructor();
      self.vertices := vertices;
      self.edges := edges;
      return self;
   }

   dijkstra: (start