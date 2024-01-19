```groovy
class Entity {
    String id
    String name
    Date createdDate
    Date lastModifiedDate
}

class Address {
    String street
    String city
    String state
    String zip
}

class Person extends Entity {
    String firstName
    String lastName
    Date dateOfBirth
    Address address
}

class Company extends Entity {
    String businessName
    String legalName
    Address address
}

class Order {
    String id
    Person customer
    Company supplier
    Date orderDate
    Date deliveryDate
    List<OrderItem> items
}

class OrderItem {
    Order order
    Product product
    int quantity
    double price
}

class Product {
    String id
    String name
    String description
    double price
}

class Category {
    String id
    String name
    String description
}

class ProductCategory {
    Product product
    Category category
}

// Usage

// Create a new person
def person = new Person(
        firstName: 'John',
        lastName: 'Doe',
        dateOfBirth: new Date('1980-10-21'),
        address: new Address(
                street: '123 Main Street',
                city: 'Anytown',
                state: 'CA',
                zip: '12345'
        )
)

// Create a new company
def company = new Company(
        businessName: 'Acme Corporation',
        legalName: 'Acme Corp.',
        address: new Address(
                street: '1000 Corporate Way',
                city: 'Anytown',
                state: 'CA',
                zip: '54321'
        )
)

// Create a new order
def order = new Order(
        customer: person,
        supplier: company,
        orderDate: new Date(),
        deliveryDate: new Date() + 7,
        items: [
                new OrderItem(
                        product: new Product(id: 'P1', name: 'Product 1', description: 'Description of Product 1', price: 100.00),
                        quantity: 2,
                        price: 200.00
                ),
                new OrderItem(
                        product: new Product(id: 'P2', name: 'Product 2', description: 'Description of Product 2', price: 50.00),
                        quantity: 1,
                        price: 50.00
                )
        ]
)

// Save the order to the database
order.save()

// Retrieve the order from the database
def orderFromDB = Order.findById(order.id)

// Print the order details
println "Order ID: ${orderFromDB.id}"
println "Customer: ${orderFromDB.customer.firstName} ${orderFromDB.customer.lastName}"
println "Supplier: ${orderFromDB.supplier.businessName}"
println "Order Date: ${orderFromDB.orderDate}"
println "Delivery Date: ${orderFromDB.deliveryDate}"
println "Items:"
orderFromDB.items.each { item ->
    println " - ${item.product.name} x ${item.quantity} = ${item.price}"
}

// Total price of the order
def totalPrice = orderFromDB.items.sum { it.price }
println "Total Price: ${totalPrice}"
```

This code demonstrates the use of Groovy's object-oriented programming features, including classes, inheritance, and polymorphism. It also shows how to use Groovy's closures and list comprehension features to make the code more concise and readable.