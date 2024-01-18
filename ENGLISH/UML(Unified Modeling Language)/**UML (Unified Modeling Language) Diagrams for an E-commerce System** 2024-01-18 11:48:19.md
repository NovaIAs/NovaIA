**Class Diagram:**

```
+----------------+
| **Customer**    |
+----------------+
| - id: int    |
| - name: string |
| - email: string |
+----------------+

+-----------------+
| **Product**      |
+-----------------+
| - id: int       |
| - name: string  |
| - description: string |
| - price: decimal |
+-----------------+

+---------------------+
| **Order**             |
+---------------------+
| - id: int            |
| - customer_id: int   |
| - product_id: int    |
| - quantity: int      |
| - total_price: decimal |
+---------------------+
```

**Sequence Diagram:**

```
Customer -> System: Send Order Request
System -> Database: Get Customer Data
System -> Database: Get Product Data
System -> Database: Calculate Total Price
System -> Database: Create Order
System -> Customer: Send Order Confirmation
```

**Activity Diagram:**

```
Start
-> Get Customer Data
-> Get Product Data
-> Calculate Total Price
-> Create Order
-> Send Order Confirmation
-> End
```

**Use Case Diagram:**

```
+-------------------+
| **Customer**      |
+-------------------+
| - Place Order    |
| - View Orders   |
+-------------------+

+-------------------+
| **System**        |
+-------------------+
| - Receive Order   |
| - Process Order  |
| - Send Confirmation |
+-------------------+
```

**Explanation:**

The class diagram defines the classes involved in the system: `Customer`, `Product`, and `Order`. The sequence diagram shows the interactions between these classes when a customer places an order. The activity diagram shows the activities involved in processing an order. The use case diagram shows the interactions between the customer and the system.

This code demonstrates the use of UML to model a simple e-commerce system. The code includes a class diagram, sequence diagram, activity diagram, and use case diagram. These diagrams provide a comprehensive view of the system, showing the classes, interactions, activities, and use cases involved. This information can be used to design and implement the system, as well as to document and communicate its functionality.