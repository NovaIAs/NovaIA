```
+--------------------------------------------------------------------------------------------------------------------+
| Class Diagram: Supply Chain Management System                                                                         |
+--------------------------------------------------------------------------------------------------------------------+

  ------------------------------------------
 | Supplier                             |
 |----------------------------------------|
 |  + id: int                           |
 |  + name: string                       |
 |  + address: string                    |
 |  + phone: string                      |
 |  + email: string                      |
 |  + products: [Product]                |
 |----------------------------------------|
 |  + addProduct(product: Product): void  |
 |  + removeProduct(product: Product): void |
 |  + updateProduct(product: Product): void |
 |  + getProductById(id: int): Product    |
 |  + getAllProducts(): [Product]        |
 |----------------------------------------|

  ------------------------------------------
 | Product                             |
 |----------------------------------------|
 |  + id: int                           |
 |  + name: string                       |
 |  + description: string                 |
 |  + price: double                      |
 |  + quantity: int                      |
 |  + supplier: Supplier                 |
 |----------------------------------------|
 |  + getSupplier(): Supplier             |
 |  + setSupplier(supplier: Supplier): void |
 |  + addSupplier(supplier: Supplier): void |
 |  + removeSupplier(supplier: Supplier): void |
 |  + getAllSuppliers(): [Supplier]       |
 |----------------------------------------|

  ------------------------------------------
 | Customer                           |
 |----------------------------------------|
 |  + id: int                           |
 |  + name: string                       |
 |  + address: string                    |
 |  + phone: string                      |
 |  + email: string                      |
 |  + orders: [Order]                   |
 |----------------------------------------|
 |  + addOrder(order: Order): void        |
 |  + removeOrder(order: Order): void      |
 |  + updateOrder(order: Order): void      |
 |  + getOrderById(id: int): Order        |
 |  + getAllOrders(): [Order]            |
 |----------------------------------------|

  ------------------------------------------
 | Order                              |
 |----------------------------------------|
 |  + id: int                           |
 |  + customer: Customer                 |
 |  + products: [Product]                |
 |  + total: double                      |
 |  + status: string                     |
 |----------------------------------------|
 |  + getCustomer(): Customer              |
 |  + setCustomer(customer: Customer): void |
 |  + addProduct(product: Product): void    |
 |  + removeProduct(product: Product): void |
 |  + updateProduct(product: Product): void |
 |  + getProductById(id: int): Product      |
 |  + getAllProducts(): [Product]          |
 |----------------------------------------|

  ------------------------------------------
 | Database                          |
 |----------------------------------------|
 |  + suppliers: [Supplier]             |
 |  + products: [Product]               |
 |  + customers: [Customer]             |
 |  + orders: [Order]                   |
 |----------------------------------------|
 |  + addSupplier(supplier: Supplier): void |
 |  + removeSupplier(supplier: Supplier): void |
 |  + updateSupplier(supplier: Supplier): void |
 |  + getSupplierById(id: int): Supplier    |
 |  + getAllSuppliers(): [Supplier]       |
 |----------------------------------------|
 |  + addProduct(product: Product): void    |
 |  + removeProduct(product: Product): void |
 |  + updateProduct(product: Product): void |
 |  + getProductById(id: int): Product      |
 |  + getAllProducts(): [Product]          |
 |----------------------------------------|
 |  + addCustomer(customer: Customer): void |
 |  + removeCustomer(customer: Customer): void |
 |  + updateCustomer(customer: Customer): void |
 |  + getCustomerById(id: int): Customer    |
 |  + getAllCustomers(): [Customer]       |
 |----------------------------------------|
 |  + addOrder(order: Order): void         |
 |  + removeOrder(order: Order): void       |
 |  + updateOrder(order: Order): void       |
 |  + getOrderById(id: int): Order         |
 |  + getAllOrders(): [Order]             |
 +----------------------------------------|

  ------------------------------------------
 | User Interface                    |
 |----------------------------------------|
 |  + displaySuppliers(): void            |
 |  + displayProducts(): void             |
 |  + displayCustomers(): void           |
 |  + displayOrders(): void              |
 |  + addSupplier(): void                |
 |  + addProduct(): void                 |
 |  + addCustomer(): void                |
 |  + addOrder(): void                   |
 |  + updateSupplier(): void              |
 |  + updateProduct(): void               |
 |  + updateCustomer(): void              |
 |  + updateOrder(): void                |
 |  + deleteSupplier(): void              |
 |  + deleteProduct(): void               |
 |  + deleteCustomer(): void              |
 |  + deleteOrder(): void                |
 +----------------------------------------|

  ------------------------------------------
 | Main                              |
 |----------------------------------------|
 |  + main(): void                       |
 |----------------------------------------|
 |  + createDatabase(): void              |
 |  + createSuppliers(): void             |
 |  + createProducts(): void              |
 |  + createCustomers(): void             |
 |  + createOrders(): void               |
 |  + displayMenu(): void                 |
 |  + handleUserInput(): void             |
 |  + executeAction(): void               |
 |----------------------------------------|

```

Explanation:

* The class diagram represents a supply chain management system.
* The Supplier class represents a supplier of products.
* The Product class represents a product sold by the suppliers.
* The Customer class represents a customer who buys products from the suppliers.
* The Order class represents an order placed by a customer.
* The Database class represents the database that stores the data about suppliers, products, customers, and orders.
* The User Interface class represents the user interface that allows the user to interact with the system.
* The Main class is the entry point of the application.
* The main() method creates the database, creates the suppliers, products, customers, and orders, displays the menu, handles the user input, and executes the action.