```
+----------------------------------------------------------------+
|                                                                |
|             +--------------+      +--------------+            |
|             |              |      |              |            |
|             |      User     |      |      Order    |            |
|             |              |      |              |            |
|             +--------------+      +--------------+            |
|                                                                |
+----------------------------------------------------------------+
                   /                                     \
                  /                                       \
                 /                                         \
                /                                           \
            +----------------------------------------------------+
            |                                                    |
            |                   Product                         |
            |                                                    |
            +----------------------------------------------------+
                 \                                         /
                  \                                       /
                   \                                     /
                    +--------------------------------+
                    |                                |
                    |        Order Line Item         |
                    |                                |
                    +--------------------------------+

```

**Explanation:**

* **User:** The `User` class represents a user of the system. This class could contain information such as the user's name, email address, and password.
* **Order:** The `Order` class represents an order that a user has placed. This class could contain information such as the order date, the total amount of the order, and the shipping address.
* **Product:** The `Product` class represents a product that is available for purchase. This class could contain information such as the product name, the product description, and the product price.
* **Order Line Item:** The `Order Line Item` class represents a single item in an order. This class could contain information such as the product name, the quantity of the product ordered, and the price of the product.

**Relationships:**

* **User:** A user can place many orders.
* **Order:** An order can contain many order line items.
* **Product:** A product can be included in many order line items.
* **Order Line Item:** An order line item is associated with a single order and a single product.

This is just a simple example of a UML class diagram. UML class diagrams can be used to model complex systems with many classes and relationships.