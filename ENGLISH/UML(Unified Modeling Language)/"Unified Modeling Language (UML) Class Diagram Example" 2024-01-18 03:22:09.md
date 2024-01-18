```
    +-------------------------------------------------------------------------------------------+
    |                                                                                           |
    |                 +----------------------------------------------------------+                 |
    |                 |                                                          |                 |
    |  +--------------+ +--------------+  +---------------------------+  +--------------+  |
    |  |  Order Item  | |  Order Item  |  |           Order           |  |  Order Item  |  |
    |  +--------------+ +--------------+  +---------------------------+  +--------------+  |
    |  |              | |              |  |                           |  |              |  |
    |  |              | |              |  |     +-----------------+    |  |              |  |
    |  |     id      | |     id      |  |  /   |                 |   \  |  |     id      |  |
    |  | item_name   | | item_name   |  | |    |                 |    | |  | item_name   |  |
    |  |   quantity  | |   quantity  |  |  |    |     Order      |    |  |  |   quantity  |  |
    |  |    price    | |    price    |  | |    |                 |    | |  |    price    |  |
    |  |              | |              |  |  \   |                 |   /  |  |              |  |
    |  +--------------+ +--------------+  +---------------------------+  +--------------+  |
    |                                                                                           |
    +-------------------------------------------------------------------------------------------+

    +-------------------------------------------------------------------------------------------+
    |                                                                                           |
    |                 +----------------------------------------------------------+                 |
    |                 |                                                          |                 |
    |  +--------------+ +--------------+  +--------------------------+  +--------------------+  |
    |  |  Order Item  | |  Order Item  |  |           Order           |  |       Customer     |  |
    |  +--------------+ +--------------+  +--------------------------+  +--------------------+  |
    |  |              | |              |  |                           |  |                   |  |
    |  |     id      | |     id      |  |     +-----------------+    |  |       id        |  |
    |  | item_name   | | item_name   |  |  /   |                 |   \  |  |      name      |  |
    |  |   quantity  | |   quantity  |  | |    |                 |    | |  |    address    |  |
    |  |    price    | |    price    |  |  |    |     Order      |    | |  |     phone     |  |
    |  |              | |              |  | |    |                 |    | |  |   email_id   |  |
    |  +--------------+ +--------------+  +--------------------------+  +--------------------+  |
    |                                                                                           |
    +-------------------------------------------------------------------------------------------+
```

This is an example of a UML class diagram. It represents the relationships between various classes in a software system. In this diagram, we have three classes: Order, OrderItem, and Customer.

The Order class represents an order placed by a customer. It has attributes such as id, order_date, total_amount, and customer_id.

The OrderItem class represents an item in an order. It has attributes such as id, item_name, quantity, and price.

The Customer class represents a customer who places orders. It has attributes such as id, name, address, phone, and email_id.

The relationships between these classes are shown by the lines connecting them. The line between the Order and OrderItem classes indicates that an Order can have multiple OrderItems. The line between the Order and Customer classes indicates that an Order is placed by a Customer.

This is just a simple example of a UML class diagram. UML is a very powerful tool for modeling software systems. It can be used to represent a wide variety of concepts, such as classes, objects, relationships, and processes.