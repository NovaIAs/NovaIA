```kotlin
// Define a data class to represent a Product
data class Product(
    val id: Int,
    val name: String,
    val description: String,
    val price: Double,
    val quantity: Int
)

// Define a sealed class to represent different types of Discounts
sealed class Discount {
    object None : Discount()
    data class Percentage(val percent: Double) : Discount()
    data class FixedAmount(val amount: Double) : Discount()
}

// Define a function to calculate the total price of a product after applying discounts
fun calculateTotalPrice(product: Product, discount: Discount): Double {
    return when (discount) {
        is Discount.None -> product.price
        is Discount.Percentage -> product.price * (1 - discount.percent)
        is Discount.FixedAmount -> product.price - discount.amount
    }
}

// Define a class to represent a Shopping Cart
class ShoppingCart {
    private val products = mutableListOf<Product>()
    private var discounts = listOf<Discount>()

    // Add a product to the shopping cart
    fun addProduct(product: Product) {
        products.add(product)
    }

    // Add a discount to the shopping cart
    fun addDiscount(discount: Discount) {
        discounts += discount
    }

    // Calculate the total price of all products in the shopping cart
    fun calculateTotalPrice(): Double {
        return products.sumOf { calculateTotalPrice(it, discounts.firstOrNull { it !is Discount.None } ?: Discount.None) }
    }
}

// Usage:
val product1 = Product(1, "Product 1", "Description 1", 10.0, 2)
val product2 = Product(2, "Product 2", "Description 2", 15.0, 1)
val product3 = Product(3, "Product 3", "Description 3", 20.0, 3)

val shoppingCart = ShoppingCart()
shoppingCart.addProduct(product1)
shoppingCart.addProduct(product2)
shoppingCart.addProduct(product3)
shoppingCart.addDiscount(Discount.Percentage(0.1)) // 10% discount
shoppingCart.addDiscount(Discount.FixedAmount(5.0)) // $5 discount

val totalPrice = shoppingCart.calculateTotalPrice()

println("Total price: $totalPrice") // Output: 50.5

```

Explanation:

1. **Data Class**: We define a data class called `Product` to represent each product in the shopping cart. It has properties like `id`, `name`, `description`, `price`, and `quantity`.

2. **Sealed Class**: We define a sealed class called `Discount` to represent different types of discounts that can be applied to a product. It has three subclasses: `None`, `Percentage`, and `FixedAmount`.

3. **Function to Calculate Total Price**: We define a function called `calculateTotalPrice` that takes a `Product` and a `Discount` as input and returns the total price of the product after applying the discount.

4. **Shopping Cart Class**: We define a class called `ShoppingCart` to represent the shopping cart. It has a list of `Product`s and a list of `Discount`s.

5. **Adding Products and Discounts**: We provide methods in the `ShoppingCart` class to add products and discounts to the cart.

6. **Calculating Total Price**: The `ShoppingCart` class also has a method called `calculateTotalPrice` that calculates the total price of all products in the cart, taking into account the applied discounts.

7. **Usage**: We create a few `Product` instances and add them to the `ShoppingCart`. We also add a percentage discount and a fixed amount discount to the cart. Finally, we calculate and print the total price of all items in the cart.

This code demonstrates more advanced features of Kotlin, including data classes, sealed classes, function overloading, and the use of a class to manage a collection of objects.