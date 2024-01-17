```
+--------------------------------------------------------+
|                                                        |
|               +-------------------+                       |
|               |                   |                       |
|   +-----------+                   |   +------------------+  |
|   | Customer |    +---------------+  |   | Warehouse |      |  |
|   +-----------+    |   Inventory   |  |   +--------------+   |  |
|                   |   +-----------+  |                   |   |
|                   |                   |                   |   |
|                   +-------------------+                   |   |
|                                                        |
+--------------------------------------------------------+

Customer:
  - id
  - name
  - address
  - phone

Warehouse:
  - id
  - location
  - capacity

Inventory:
  - id
  - product_id
  - quantity

Order:
  - id
  - customer_id
  - warehouse_id
  - product_id
  - quantity

Product:
  - id
  - name
  - price

+--------------------------------------------------------+
|                                                        |
|               +-------------------+                       |
|               |                   |                       |
|   +-----------+                   |   +------------------+  |
|   | Customer |    +---------------+  |   | Order |           |  |
|   +-----------+    |   Order   |  |   +------------------+   |
|                   |   +-----------+  |                   |   |
|                   |                   |                   |   |
|                   +-------------------+                   |   |
|                                                        |
+--------------------------------------------------------+

Customer:
  - id
  - name
  - address
  - phone

Order:
  - id
  - customer_id
  - product_id
  - quantity

Product:
  - id
  - name
  - price

+--------------------------------------------------------+
|                                                        |
|               +-------------------+                       |
|               |                   |                       |
|   +-----------+                   |   +------------------+  |
|   | Customer |    +---------------+  |   | Product |        |  |
|   +-----------+    |   Order   |  |   +--------------+   |  |
|                   |   +-----------+  |                   |   |
|                   |                   |                   |   |
|                   +-------------------+                   |   |
|                                                        |
+--------------------------------------------------------+

Customer:
  - id
  - name
  - address
  - phone

Order:
  - id
  - customer_id
  - product_id
  - quantity

Product:
  - id
  - name
  - price

+--------------------------------------------------------+
|                                                        |
|               +-------------------+                       |
|               |                   |                       |
|   +-----------+                   |   +------------------+  |
|   | Customer |    +---------------+  |   | Warehouse |      |  |
|   +-----------+    |   Order   |  |   +--------------+   |  |
|                   |   +-----------+  |                   |   |
|                   |                   |                   |   |
|                   +-------------------+                   |   |
|                                                        |
+--------------------------------------------------------+

Customer:
  - id
  - name
  - address
  - phone

Order:
  - id
  - customer_id
  - product_id
  - quantity

Warehouse:
  - id
  - location
  - capacity

+--------------------------------------------------------+
|                                                        |
|               +-------------------+                       |
|               |                   |                       |
|   +-----------+                   |   +------------------+  |
|   | Customer |    +---------------+  |   | Inventory |      |  |
|   +-----------+    |   Product   |  |   +--------------+   |  |
|                   |   +-----------+  |                   |   |
|                   |                   |                   |   |
|                   +-------------------+                   |   |
|                                                        |
+--------------------------------------------------------+

Customer:
  - id
  - name
  - address
  - phone

Product:
  - id
  - name
  - price

Inventory:
  - id
  - product_id
  - quantity

+--------------------------------------------------------+
|                                                        |
|               +-------------------+                       |
|               |                   |                       |
|   +-----------+                   |   +------------------+  |
|   | Customer |    +---------------+  |   | Product |        |  |
|   +-----------+    |   Invoice   |  |   +--------------+   |  |
|                   |   +-----------+  |                   |   |
|                   |                   |                   |   |
|                   +-------------------+                   |   |
|                                                        |
+--------------------------------------------------------+

Customer:
  - id
  - name
  - address
  - phone

Invoice:
  - id
  - customer_id
  - product_id
  - quantity
  - price

Product:
  - id
  - name
  - price
```

This UML diagram represents a very complex and differentiated system. It includes multiple classes and relationships, and it would be difficult to repeat it again.

The system includes the following classes:

* Customer: A customer of the system.
* Warehouse: A warehouse where products are stored.
* Inventory: The inventory of products in a warehouse.
* Order: An order for products from a customer.
* Product: A product that is sold by the system.
* Invoice: An invoice for products that have been sold to a customer.

The relationships between the classes are as follows:

* A customer can place multiple orders.
* A warehouse can store multiple products.
* An order can be for multiple products.
* A product can be stored in multiple warehouses.
* An invoice can be for multiple products.

This is just a brief overview of the system. The actual implementation would be much more complex and would require a large amount of code.

Here are some of the benefits of using UML:

* It provides a visual representation of the system, which makes it easier to understand.
* It can be used to document the system, which makes it easier to maintain.
* It can be used to generate code, which saves time and effort.

UML is a powerful tool that can be used to design and document complex systems. It is a valuable tool for software engineers and project managers.