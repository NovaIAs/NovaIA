**Use Case Diagram:**

* **Actors:**
    * Customer
    * Product Manager
    * Salesperson
    * System Administrator
    * Warehouse Manager

* **Use Cases:**
    * Place Order
    * Manage Products
    * Track Orders
    * Manage Customers
    * Manage Inventory

**Class Diagram:**

* **Classes:**
    * Customer
        * Attributes:
            * ID
            * Name
            * Address
            * Phone Number
            * Email Address
        * Methods:
            * Place Order
            * Track Orders
            * Manage Account
    * Product
        * Attributes:
            * ID
            * Name
            * Description
            * Price
            * Quantity
        * Methods:
            * Add to Cart
            * Remove from Cart
            * Update Quantity
    * Order
        * Attributes:
            * ID
            * Customer ID
            * Product ID
            * Quantity
            * Total Price
            * Order Date
            * Shipping Address
            * Shipping Method
        * Methods:
            * Place Order
            * Track Order
            * Cancel Order
    * Salesperson
        * Attributes:
            * ID
            * Name
            * Address
            * Phone Number
            * Email Address
        * Methods:
            * Manage Products
            * Process Orders
            * Manage Customers
    * Product Manager
        * Attributes:
            * ID
            * Name
            * Address
            * Phone Number
            * Email Address
        * Methods:
            * Manage Products
            * Track Orders
            * Manage Customers
    * System Administrator
        * Attributes:
            * ID
            * Name
            * Address
            * Phone Number
            * Email Address
        * Methods:
            * Manage Users
            * Manage Roles
            * Manage Permissions
    * Warehouse Manager
        * Attributes:
            * ID
            * Name
            * Address
            * Phone Number
            * Email Address
        * Methods:
            * Manage Inventory
            * Track Orders
            * Manage Warehouse

**Sequence Diagram:**

* **Scenario:** A customer places an order for a product.

1. Customer selects a product and adds it to their cart.
2. Customer clicks on the "Place Order" button.
3. System generates an order ID and sends an order confirmation email to the customer.
4. Salesperson receives an order notification and processes the order.
5. Warehouse manager picks the product from the warehouse and prepares it for shipment.
6. Shipping company picks up the product and delivers it to the customer.
7. Customer receives the product and confirms delivery.

**Activity Diagram:**

* **Scenario:** A product manager creates a new product.

1. Product manager logs into the system.
2. Product manager clicks on the "Create New Product" button.
3. Product manager enters the product details (name, description, price, quantity).
4. Product manager clicks on the "Save" button.
5. System validates the product details and saves the product to the database.
6. Product manager receives a confirmation message.