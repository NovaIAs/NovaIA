**Program:** Grocery List Manager

**Purpose:** This program helps you manage your grocery list. You can add items to the list, remove items from the list, and print the list.

**Code:**

```cool
class GroceryList {
  items: List;

  constructor() {
    self.items := List.new();
  }

  add(item) {
    self.items.add(item);
  }

  remove(item) {
    self.items.remove(item);
  }

  print() {
    for (item in self.items) {
      print(item);
    }
  }
}

main() {
  list := GroceryList.new();

  list.add("Milk");
  list.add("Eggs");
  list.add("Bread");

  list.print();

  list.remove("Milk");

  list.print();
}
```

**Explanation:**

* The `GroceryList` class represents a grocery list. It has a `List` attribute called `items` that stores the items in the list.
* The `constructor` method initializes the `GroceryList` object by creating a new `List` object and assigning it to the `items` attribute.
* The `add` method adds an item to the `GroceryList` object by calling the `add` method of the `List` object.
* The `remove` method removes an item from the `GroceryList` object by calling the `remove` method of the `List` object.
* The `print` method prints the items in the `GroceryList` object by calling the `print` method of the `List` object.
* The `main` method creates a new `GroceryList` object, adds some items to the list, prints the list, removes an item from the list, and prints the list again.

**Output:**

```
Milk
Eggs
Bread
Eggs
Bread
```