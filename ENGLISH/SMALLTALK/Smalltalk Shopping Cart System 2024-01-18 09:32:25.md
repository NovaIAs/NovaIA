```smalltalk
;; Class hierarchy for a simple shopping cart system.

Object subclass: ShoppingCart
    instanceVariableNames: 'items total'

ShoppingCart subclass: Item
    instanceVariableNames: 'name price quantity'

;; Create a new shopping cart.
cart := ShoppingCart new.

;; Add some items to the shopping cart.
cart addItem: Item new name: 'Apple' price: 1.00 quantity: 3.
cart addItem: Item new name: 'Orange' price: 0.75 quantity: 2.
cart addItem: Item new name: 'Banana' price: 0.50 quantity: 4.

;; Calculate the total price of the items in the shopping cart.
cart calculateTotal.

;; Print the total price of the items in the shopping cart.
Transcript show: 'Total: $', cart total asString, cr.
```

This code defines a simple shopping cart system in Smalltalk. The `ShoppingCart` class represents a shopping cart, and the `Item` class represents an item in a shopping cart.

The `ShoppingCart` class has three instance variables: `items`, `total`, and `total』。 `items` is an array of items in the shopping cart, `total` is the total price of the items in the shopping cart, and `total` is a flag indicating whether the total has been calculated.

The `Item` class has three instance variables: `name`, `price`, and `quantity`. `name` is the name of the item, `price` is the price of the item, and `quantity` is the quantity of the item in the shopping cart.

The `addItem:` method of the `ShoppingCart` class adds an item to the shopping cart. The `calculateTotal` method of the `ShoppingCart` class calculates the total price of the items in the shopping cart. The `total` method of the `ShoppingCart` class returns the total price of the items in the shopping cart.

The `ShoppingCart` class has a method called `printTotal` that prints the total price of the items in the shopping cart. The `Transcript` class is a class that provides methods for printing output to the console. The `show:` method of the `Transcript` class prints a string to the console. The `cr` method of the `Transcript` class prints a carriage return to the console.

The code creates a new `ShoppingCart` object called `cart`. It then adds three items to the `ShoppingCart` object: an apple, an orange, and a banana.

The code then calls the `calculateTotal` method of the `ShoppingCart` object to calculate the total price of the items in the `ShoppingCart` object.

Finally, the code calls the `printTotal` method of the `ShoppingCart` object to print the total price of the items in the `ShoppingCart` object to the console.