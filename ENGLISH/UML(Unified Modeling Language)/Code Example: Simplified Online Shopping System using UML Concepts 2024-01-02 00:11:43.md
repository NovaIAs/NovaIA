As an AI text-based model, I can generate code snippets and provide explanations, but I cannot generate entire complex UML diagrams. However, I can definitely help you with a code snippet that demonstrates the usage of UML concepts.

Here's an example of a code snippet written in Java that represents a simplified online shopping system using UML concepts:

```java
// Class representing a customer in the online shopping system
class Customer {
    private String name;
    private String email;
    private ShoppingCart cart;

    public Customer(String name, String email) {
        this.name = name;
        this.email = email;
        this.cart = new ShoppingCart();
    }

    public void addToCart(Product product, int quantity) {
        cart.addProduct(product, quantity);
    }

    public void checkout() {
        // Perform checkout process
        cart.checkout();
    }
}

// Class representing a product available in the online shopping system
class Product {
    private String name;
    private double price;

    public Product(String name, double price) {
        this.name = name;
        this.price = price;
    }

    // Getters and setters
}

// Class representing the customer's shopping cart
class ShoppingCart {
    private List<Product> products;
    private Map<Product, Integer> quantities;

    public ShoppingCart() {
        this.products = new ArrayList<>();
        this.quantities = new HashMap<>();
    }

    public void addProduct(Product product, int quantity) {
        products.add(product);
        quantities.put(product, quantity);
    }

    public void checkout() {
        // Perform checkout process
        double total = calculateTotal();
        System.out.println("Checkout completed. Total: $" + total);
    }

    private double calculateTotal() {
        double total = 0.0;
        for (Product product : products) {
            int quantity = quantities.get(product);
            total += product.getPrice() * quantity;
        }
        return total;
    }
}
```

Explanation:
The code snippet demonstrates a simplified online shopping system using UML concepts like class diagrams and associations. 

The `Customer` class represents a customer in the system and has attributes like name, email, and a reference to their shopping cart. It has methods to add products to the cart and perform checkout.

The `Product` class represents a product available in the system and has attributes like name and price.

The `ShoppingCart` class represents the customer's shopping cart and has a list of products and a map of quantities. It has methods to add products to the cart, perform checkout, and calculate the total price.

Note that this is just a simplified example, and in a real-world scenario, you would have more classes, relationships, and functionality.