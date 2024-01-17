```
+------------------------------------------------------------------------------------------------------------------------------------------------+
|                                                                                                                                               |
|                                                                  +--------------+                                                                |
|                                                                  |              |                                                                |
|                                                                  | Customer Info |                                                                |
|                                                                  |              |                                                                |
|                                                                  +--------------+                                                                |
|                                                                                                                                               |
+------------------------------------------------------------------------------------------------------------------------------------------------+
                                                                       /             \
                                                                      /               \
                                                                     +-----------------+
                                                                     |                |
                                                                     | Customer Order |
                                                                     |                |
                                                                     +-----------------+
                                                                            /
                                                                           /
                                                                 +---------------------------+
                                                                 |                           |
                                                                 | Customer Service Inquiry |
                                                                 |                           |
                                                                 +---------------------------+
                                                                            \             /
                                                                             \           /
                                                                  +--------------+   /- - - - - \
                                                                  |              |  /             \
                                                                  | Product Info  | /               \
                                                                  |              |                |
                                                                  +--------------+                |
                                                                                                 |
                                                                                                 |  /            \
                                                                                                \/              \
                                                                     +--------------------------+
                                                                     |                          |
                                                                     | Customer Product Complaint |
                                                                     |                          |
                                                                     +--------------------------+
                                                                              \       /
                                                                               \     /
                                                                  +-------------------------------+
                                                                  |                               |
                                                                  | Customer Service Resolution  |
                                                                  |                               |
                                                                  +-------------------------------+

```

Explanation:

* **Customer Info:** This class contains information about the customer, such as name, address, and phone number.
* **Customer Order:** This class contains information about a customer's order, such as the products ordered, the quantity, and the total price.
* **Customer Service Inquiry:** This class contains information about a customer's service inquiry, such as the issue they are experiencing, the date and time of the inquiry, and the customer's contact information.
* **Product Info:** This class contains information about a product, such as the name, description, price, and availability.
* **Customer Product Complaint:** This class contains information about a customer's product complaint, such as the product in question, the nature of the complaint, and the date and time of the complaint.
* **Customer Service Resolution:** This class contains information about a customer service resolution, such as the resolution provided to the customer, the date and time of the resolution, and the customer's satisfaction with the resolution.

The relationships between the classes are as follows:

* **Customer:** A customer can place an order, make a service inquiry, or file a product complaint.
* **Customer Order:** A customer order is associated with a customer and contains information about the products ordered.
* **Customer Service Inquiry:** A customer service inquiry is associated with a customer and contains information about the issue the customer is experiencing.
* **Product Info:** A product is associated with customer orders and product complaints.
* **Customer Product Complaint:** A customer product complaint is associated with a customer, a product, and a customer service resolution.
* **Customer Service Resolution:** A customer service resolution is associated with a customer service inquiry and a customer product complaint.

This UML diagram provides a clear and concise overview of the relationships between the different classes in a customer service system. It can be used to help design and implement a new system or to improve an existing one.