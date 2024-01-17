```
**Package Diagram:**

* **Domain Model:**
    * **Entity:** `Customer`
        * Attributes:
            * `customerId` (primary key)
            * `name`
            * `address`
            * `phone`
            * `email`
    * **Entity:** `Product`
        * Attributes:
            * `productId` (primary key)
            * `name`
            * `description`
            * `price`
    * **Entity:** `Order`
        * Attributes:
            * `orderId` (primary key)
            * `customerId` (foreign key to `Customer`)
            * `orderDate`
            * `totalCost`
    * **Entity:** `OrderItem`
        * Attributes:
            * `orderItemId` (primary key)
            * `orderId` (foreign key to `Order`)
            * `productId` (foreign key to `Product`)
            * `quantity`
            * `unitPrice`

* **Data Access Layer:**
    * **Repository:** `CustomerRepository`
        * Methods:
            * `createCustomer(Customer customer)`
            * `updateCustomer(Customer customer)`
            * `deleteCustomer(int customerId)`
            * `getCustomerById(int customerId)`
            * `getAllCustomers()`
    * **Repository:** `ProductRepository`
        * Methods:
            * `createProduct(Product product)`
            * `updateProduct(Product product)`
            * `deleteProduct(int productId)`
            * `getProductById(int productId)`
            * `getAllProducts()`
    * **Repository:** `OrderRepository`
        * Methods:
            * `createOrder(Order order)`
            * `updateOrder(Order order)`
            * `deleteOrder(int orderId)`
            * `getOrderById(int orderId)`
            * `getAllOrders()`
    * **Repository:** `OrderItemRepository`
        * Methods:
            * `createOrderItem(OrderItem orderItem)`
            * `updateOrderItem(OrderItem orderItem)`
            * `deleteOrderItem(int orderItemId)`
            * `getOrderItemById(int orderItemId)`
            * `getAllOrderItems()`

* **Business Logic Layer:**
    * **Service:** `CustomerService`
        * Methods:
            * `createCustomer(Customer customer)`
            * `updateCustomer(Customer customer)`
            * `deleteCustomer(int customerId)`
            * `findCustomerById(int customerId)`
            * `findAllCustomers()`
    * **Service:** `ProductService`
        * Methods:
            * `createProduct(Product product)`
            * `updateProduct(Product product)`
            * `deleteProduct(int productId)`
            * `findProductById(int productId)`
            * `findAllProducts()`
    * **Service:** `OrderService`
        * Methods:
            * `createOrder(Order order)`
            * `updateOrder(Order order)`
            * `deleteOrder(int orderId)`
            * `findOrderById(int orderId)`
            * `findAllOrders()`
    * **Service:** `OrderItemService`
        * Methods:
            * `createOrderItem(OrderItem orderItem)`
            * `updateOrderItem(OrderItem orderItem)`
            * `deleteOrderItem(int orderItemId)`
            * `findOrderItemById(int orderItemId)`
            * `findAllOrderItems()`

* **Presentation Layer:**
    * **Controller:** `CustomerController`
        * Methods:
            * `index()`
            * `create()`
            * `store()`
            * `edit()`
            * `update()`
            * `destroy()`
    * **Controller:** `ProductController`
        * Methods:
            * `index()`
            * `create()`
            * `store()`
            * `edit()`
            * `update()`
            * `destroy()`
    * **Controller:** `OrderController`
        * Methods:
            * `index()`
            * `create()`
            * `store()`
            * `edit()`
            * `update()`
            * `destroy()`
    * **Controller:** `OrderItemController`
        * Methods:
            * `index()`
            * `create()`
            * `store()`
            * `edit()`
            * `update()`
            * `destroy()`

* **Database Schema:**

```sql
CREATE TABLE customers (
  customer_id INT NOT NULL AUTO_INCREMENT,
  name VARCHAR(255) NOT NULL,
  address VARCHAR(255),
  phone VARCHAR(255),
  email VARCHAR(255) UNIQUE,
  PRIMARY KEY (customer_id)
);

CREATE TABLE products (
  product_id INT NOT NULL AUTO_INCREMENT,
  name VARCHAR(255) NOT NULL,
  description TEXT,
  price DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (product_id)
);

CREATE TABLE orders (
  order_id INT NOT NULL AUTO_INCREMENT,
  customer_id INT NOT NULL,
  order_date TIMESTAMP,
  total_cost DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (order_id),
  FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

CREATE TABLE order_items (
  order_item_id INT NOT NULL AUTO_INCREMENT,
  order_id INT NOT NULL,
  product_id INT NOT NULL,
  quantity INT NOT NULL,
  unit_price DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (order_item_id),
  FOREIGN KEY (order_id) REFERENCES orders(order_id),
  FOREIGN KEY (product_id) REFERENCES products(product_id)
);
```

**Explanation:**

This is a complex UML diagram that represents a complete e-commerce system. The system consists of four layers: domain model, data access layer, business logic layer, and presentation layer.

The domain