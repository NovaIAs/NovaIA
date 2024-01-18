```
+------------------------------------------------------------+
|  UseCase: Manage Customer Orders                           |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Actor: Customer                                            |
+------------------------------------------------------------+
|  Description: The customer interacts with the system to place, |
|               view, and modify orders.                      |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  UseCase: Manage Products                                   |
+------------------------------------------------------------+
|  Actor: Product Manager                                     |
+------------------------------------------------------------+
|  Description: The product manager interacts with the system to |
|               add, update, and remove products.               |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  UseCase: Manage Inventory                                  |
+------------------------------------------------------------+
|  Actor: Warehouse Manager                                   |
+------------------------------------------------------------+
|  Description: The warehouse manager interacts with the system |
|               to track inventory levels and manage stock.       |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  UseCase: Process Orders                                    |
+------------------------------------------------------------+
|  Actor: Order Processor                                     |
+------------------------------------------------------------+
|  Description: The order processor interacts with the system to |
|               process customer orders, including generating     |
|               invoices and shipping orders.                   |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  UseCase: Generate Reports                                   |
+------------------------------------------------------------+
|  Actor: Manager                                              |
+------------------------------------------------------------+
|  Description: The manager interacts with the system to        |
|               generate reports on sales, inventory, and        |
|               customer orders.                                |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Class: Customer                                            |
+------------------------------------------------------------+
|  Attributes:                                                |
|    - Customer ID                                          |
|    - Name                                                  |
|    - Address                                                |
|    - Phone Number                                          |
|    - Email Address                                         |
+------------------------------------------------------------+
|  Operations:                                                |
|    - Place Order                                            |
|    - View Order                                             |
|    - Modify Order                                            |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Class: Product                                             |
+------------------------------------------------------------+
|  Attributes:                                                |
|    - Product ID                                            |
|    - Name                                                  |
|    - Description                                            |
|    - Price                                                 |
|    - Quantity                                               |
+------------------------------------------------------------+
|  Operations:                                                |
|    - Add Product                                             |
|    - Update Product                                          |
|    - Remove Product                                          |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Class: Inventory                                            |
+------------------------------------------------------------+
|  Attributes:                                                |
|    - Product ID                                            |
|    - Quantity                                               |
|    - Location                                               |
+------------------------------------------------------------+
|  Operations:                                                |
|    - Add Stock                                               |
|    - Remove Stock                                            |
|    - Transfer Stock                                          |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Class: Order                                              |
+------------------------------------------------------------+
|  Attributes:                                                |
|    - Order ID                                              |
|    - Customer ID                                            |
|    - Product ID                                            |
|    - Quantity                                               |
|    - Order Date                                             |
|    - Shipping Address                                        |
|    - Total Price                                            |
+------------------------------------------------------------+
|  Operations:                                                |
|    - Place Order                                            |
|    - Process Order                                           |
|    - Ship Order                                             |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Class: Invoice                                            |
+------------------------------------------------------------+
|  Attributes:                                                |
|    - Invoice ID                                            |
|    - Order ID                                              |
|    - Customer ID                                            |
|    - Total Price                                            |
|    - Payment Method                                          |
|    - Invoice Date                                           |
+------------------------------------------------------------+
|  Operations:                                                |
|    - Generate Invoice                                        |
|    - Send Invoice                                            |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Class: Report                                             |
+------------------------------------------------------------+
|  Attributes:                                                |
|    - Report Type                                            |
|    - Start Date                                             |
|    - End Date                                               |
|    - Data                                                  |
+------------------------------------------------------------+
|  Operations:                                                |
|    - Generate Report                                         |
|    - Print Report                                           |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Sequence Diagram: Place Order                              |
+------------------------------------------------------------+
|  Actors:                                                   |
|    - Customer                                               |
|    - Order Processor                                         |
+------------------------------------------------------------+
|  Flow of Events:                                           |
|    1. Customer selects products and adds them to the shopping |
|       cart.                                                   |
|    2. Customer clicks the "Place Order" button.                 |
|    3. Order Processor receives the order and validates it.      |
|    4. Order Processor generates an invoice and sends it to the |
|       customer.                                                |
|    5. Order Processor ships the order to the customer.          |
|    6. Customer receives the order and verifies it.             |
+------------------------------------------------------------+

+------------------------------------------------------------+
|  Sequence Diagram: Generate Report                            |
+------------------------------------------------------------+
|  Actors:                                                   |
|    - Manager                                                |
|    - Report Generator                                        |
+------------------------------------------------------------+
|  Flow of Events:                                           |
|    1. Manager selects the report type, start date, and end date.|
|    2. Report Generator generates the report and saves it as a |
|       PDF file.                                              |
|    3. Manager opens the PDF file and prints the report.        |
+------------------------------------------------------------+